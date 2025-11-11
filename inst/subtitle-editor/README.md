# Subtitle Editor

Standalone video subtitle editor with full seeking support.

## Installation

No installation required - just Node.js (version 12 or higher).

## Usage

```bash
node server.js <video-file> <srt-file>
```

### Example:

```bash
node server.js my_video.mp4 my_subtitles.srt
```

The app will automatically open in your browser at `http://localhost:3000`.

## Features

- ✅ Full video seeking/scrubbing support
- ✅ Click subtitle cards to jump to timestamps
- ✅ Auto-highlighting of current subtitle
- ✅ Auto-scrolling subtitle list
- ✅ Edit subtitles inline
- ✅ Save to custom filename
- ✅ Works with all video formats (MP4, WebM, MOV, etc.)

## Keyboard Shortcuts

- Use the video player's native controls (space to play/pause, arrow keys to seek)
- Click any subtitle to jump to that time

## Troubleshooting

**Video won't play:**
- Make sure the video file path is correct
- Try using an absolute path instead of relative

**Port 3000 already in use:**
- Edit `server.js` and change `const PORT = 3000;` to another port number

**SRT file not found:**
- Make sure the SRT file path is correct
- The file must exist before starting the server

## Stopping the Server

Press `Ctrl+C` in the terminal where the server is running.
