#!/usr/bin/env node

const http = require('http');
const fs = require('fs');
const path = require('path');
const url = require('url');

// Get command line arguments
const args = process.argv.slice(2);
if (args.length < 2) {
  console.log('Usage: node server.js <video-file> <srt-file>');
  console.log('Example: node server.js my_video.mp4 my_subtitles.srt');
  process.exit(1);
}

const videoPath = path.resolve(args[0]);
const srtPath = path.resolve(args[1]);

// Verify files exist
if (!fs.existsSync(videoPath)) {
  console.error('Error: Video file not found:', videoPath);
  process.exit(1);
}
if (!fs.existsSync(srtPath)) {
  console.error('Error: SRT file not found:', srtPath);
  process.exit(1);
}

console.log('Video file:', videoPath);
console.log('SRT file:', srtPath);

// MIME type detection
function getMimeType(filePath) {
  const ext = path.extname(filePath).toLowerCase();
  const mimeTypes = {
    '.mp4': 'video/mp4',
    '.webm': 'video/webm',
    '.ogg': 'video/ogg',
    '.ogv': 'video/ogg',
    '.mov': 'video/quicktime',
    '.avi': 'video/x-msvideo',
    '.mkv': 'video/x-matroska'
  };
  return mimeTypes[ext] || 'video/mp4';
}

const videoMimeType = getMimeType(videoPath);

// Create server
const server = http.createServer((req, res) => {
  const parsedUrl = url.parse(req.url, true);
  const pathname = parsedUrl.pathname;

  // CORS headers
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Range, Content-Type');

  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }

  // Serve the HTML UI
  if (pathname === '/' || pathname === '/index.html') {
    const html = fs.readFileSync(path.join(__dirname, 'index.html'), 'utf8');
    res.writeHead(200, { 'Content-Type': 'text/html' });
    res.end(html);
    return;
  }

  // Serve video with range request support
  if (pathname === '/video') {
    const stat = fs.statSync(videoPath);
    const fileSize = stat.size;
    const range = req.headers.range;

    if (range) {
      // Parse range header
      const parts = range.replace(/bytes=/, '').split('-');
      const start = parseInt(parts[0], 10);
      const end = parts[1] ? parseInt(parts[1], 10) : fileSize - 1;
      const chunksize = (end - start) + 1;
      const file = fs.createReadStream(videoPath, { start, end });

      res.writeHead(206, {
        'Content-Range': `bytes ${start}-${end}/${fileSize}`,
        'Accept-Ranges': 'bytes',
        'Content-Length': chunksize,
        'Content-Type': videoMimeType
      });
      file.pipe(res);
    } else {
      // No range, send entire file
      res.writeHead(200, {
        'Content-Length': fileSize,
        'Content-Type': videoMimeType,
        'Accept-Ranges': 'bytes'
      });
      fs.createReadStream(videoPath).pipe(res);
    }
    return;
  }

  // Get SRT content
  if (pathname === '/srt') {
    const srtContent = fs.readFileSync(srtPath, 'utf8');
    res.writeHead(200, { 'Content-Type': 'text/plain; charset=utf-8' });
    res.end(srtContent);
    return;
  }

  // Get save directory info
  if (pathname === '/save-info') {
    const saveDir = path.dirname(srtPath);
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      directory: saveDir,
      defaultFilename: path.basename(srtPath)
    }));
    return;
  }

  // Save SRT content
  if (pathname === '/save' && req.method === 'POST') {
    let body = '';
    req.on('data', chunk => {
      body += chunk.toString();
    });
    req.on('end', () => {
      try {
        const data = JSON.parse(body);
        const filename = data.filename || path.basename(srtPath);
        const outputPath = path.join(path.dirname(srtPath), filename);

        fs.writeFileSync(outputPath, data.content, 'utf8');

        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({
          success: true,
          message: `Saved successfully!`,
          fullPath: outputPath
        }));
      } catch (err) {
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ success: false, message: err.message }));
      }
    });
    return;
  }

  // 404
  res.writeHead(404);
  res.end('Not found');
});

const PORT = 3000;
server.listen(PORT, () => {
  console.log(`\nSubtitle Editor running at http://localhost:${PORT}`);
  console.log('Press Ctrl+C to stop\n');

  // Auto-open browser (optional)
  const open = require('child_process').exec;
  const url = `http://localhost:${PORT}`;
  const start = process.platform === 'darwin' ? 'open' : process.platform === 'win32' ? 'start' : 'xdg-open';
  open(`${start} ${url}`);
});

// Graceful shutdown
process.on('SIGINT', () => {
  console.log('\nShutting down server...');
  server.close(() => {
    console.log('Server stopped');
    process.exit(0);
  });
});
