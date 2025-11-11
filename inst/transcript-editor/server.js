#!/usr/bin/env node

const http = require('http');
const fs = require('fs');
const path = require('path');
const { exec } = require('child_process');

// Get command line arguments
const args = process.argv.slice(2);
if (args.length < 3) {
  console.error('Usage: node server.js <audio_file> <transcript_json> <txt_file>');
  process.exit(1);
}

const audioFile = args[0];
const transcriptJson = args[1];
const txtFile = args[2];

// Validate files exist
if (!fs.existsSync(audioFile)) {
  console.error(`Audio file not found: ${audioFile}`);
  process.exit(1);
}

if (!fs.existsSync(transcriptJson)) {
  console.error(`Transcript JSON not found: ${transcriptJson}`);
  process.exit(1);
}

const PORT = 3789;

// MIME types for different file extensions
const mimeTypes = {
  '.html': 'text/html',
  '.js': 'text/javascript',
  '.css': 'text/css',
  '.json': 'application/json',
  '.mp3': 'audio/mpeg',
  '.wav': 'audio/wav',
  '.m4a': 'audio/mp4',
  '.mp4': 'video/mp4',
  '.webm': 'audio/webm',
  '.ogg': 'audio/ogg',
};

const server = http.createServer((req, res) => {
  // Enable CORS
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Range');

  if (req.method === 'OPTIONS') {
    res.writeHead(200);
    res.end();
    return;
  }

  console.log(`${req.method} ${req.url}`);

  // Serve the HTML interface
  if (req.url === '/' || req.url === '/index.html') {
    const htmlPath = path.join(__dirname, 'index.html');
    fs.readFile(htmlPath, (err, data) => {
      if (err) {
        res.writeHead(500);
        res.end(`Error loading index.html: ${err.message}`);
        return;
      }
      res.writeHead(200, { 'Content-Type': 'text/html' });
      res.end(data);
    });
    return;
  }

  // Serve the transcript JSON data
  if (req.url === '/transcript-data') {
    fs.readFile(transcriptJson, (err, data) => {
      if (err) {
        res.writeHead(500);
        res.end(`Error loading transcript: ${err.message}`);
        return;
      }
      res.writeHead(200, { 'Content-Type': 'application/json' });
      res.end(data);
    });
    return;
  }

  // Serve audio file with range request support (required for seeking)
  if (req.url === '/audio') {
    const stat = fs.statSync(audioFile);
    const fileSize = stat.size;
    const range = req.headers.range;

    const ext = path.extname(audioFile).toLowerCase();
    const mimeType = mimeTypes[ext] || 'application/octet-stream';

    if (range) {
      // Handle range request for seeking
      const parts = range.replace(/bytes=/, '').split('-');
      const start = parseInt(parts[0], 10);
      const end = parts[1] ? parseInt(parts[1], 10) : fileSize - 1;
      const chunksize = (end - start) + 1;
      const file = fs.createReadStream(audioFile, { start, end });

      res.writeHead(206, {
        'Content-Range': `bytes ${start}-${end}/${fileSize}`,
        'Accept-Ranges': 'bytes',
        'Content-Length': chunksize,
        'Content-Type': mimeType,
      });
      file.pipe(res);
    } else {
      // Serve entire file
      res.writeHead(200, {
        'Content-Length': fileSize,
        'Content-Type': mimeType,
        'Accept-Ranges': 'bytes',
      });
      fs.createReadStream(audioFile).pipe(res);
    }
    return;
  }

  // Get save directory info
  if (req.url === '/save-info') {
    const saveDir = path.dirname(txtFile);
    res.writeHead(200, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      directory: saveDir,
      defaultFilename: path.basename(txtFile),
      fullPath: txtFile
    }));
    return;
  }

  // Save edited transcript
  if (req.url === '/save' && req.method === 'POST') {
    let body = '';
    req.on('data', chunk => {
      body += chunk.toString();
    });
    req.on('end', () => {
      try {
        const data = JSON.parse(body);
        const filename = data.filename || path.basename(txtFile);
        const outputPath = path.join(path.dirname(txtFile), filename);

        fs.writeFileSync(outputPath, data.text, 'utf8');
        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({
          success: true,
          message: 'Transcript saved successfully!',
          fullPath: outputPath
        }));
        console.log(`Transcript saved to: ${outputPath}`);
      } catch (err) {
        res.writeHead(500, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({ success: false, message: err.message }));
      }
    });
    return;
  }

  // 404 for anything else
  res.writeHead(404);
  res.end('Not found');
});

server.listen(PORT, () => {
  console.log(`\nTranscript Editor Server running at http://localhost:${PORT}/`);
  console.log(`Audio file: ${audioFile}`);
  console.log(`Transcript: ${txtFile}`);
  console.log('\nOpening browser...\n');

  // Open browser automatically
  const url = `http://localhost:${PORT}/`;
  const start = process.platform === 'darwin' ? 'open' :
                process.platform === 'win32' ? 'start' : 'xdg-open';
  exec(`${start} ${url}`);
});

// Handle shutdown gracefully
process.on('SIGINT', () => {
  console.log('\n\nShutting down server...');
  server.close(() => {
    console.log('Server closed.');
    process.exit(0);
  });
});
