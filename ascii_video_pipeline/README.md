# ASCII Video Pipeline

This project converts video files into ASCII art animations.

## Requirements

You must have the following command-line tools installed to run the `convert_video.sh` script:

- **ffmpeg**: Used to extract image frames from a video file.
- **jp2a**: Used to convert image frames into ASCII art.

### Installation (macOS)

```bash
brew install ffmpeg jp2a
```

## Usage

### Make the script executable

Before running the script for the first time, you need to give your system permission to execute it. You only need to do this once.

```bash
chmod +x convert_video.sh
```
