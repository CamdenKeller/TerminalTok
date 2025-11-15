#!/bin/bash

# --- CONFIGURATION ---
TARGET_FPS=10      # Frames-per-second to extract
TARGET_WIDTH=80    # The width in characters for the ASCII art
DELIMITER="---FRAME_BREAK---" # The text that will separate each frame.

# --- SCRIPT LOGIC ---

# 1. Check if a video file was provided
if [ -z "$1" ]; then
  echo "Usage: ./convert_video.sh [your_video_file.mp4]"
  exit 1
fi

# 2. Set up file and directory names
VIDEO_FILE="$1"
# Gets the filename without the extension (e.g., "my_video")
BASENAME=$(basename "$VIDEO_FILE" .${VIDEO_FILE##*.}) 

# We will create temporary directories to hold the frames
FRAME_DIR="temp_${BASENAME}_frames"
ASCII_DIR="temp_${BASENAME}_ascii"
OUTPUT_FILE="${BASENAME}.ascii"

# 3. Start clean: remove old temp files and output
echo "Cleaning up old files..."
rm -rf "$FRAME_DIR" "$ASCII_DIR" "$OUTPUT_FILE"
mkdir -p "$FRAME_DIR"
mkdir -p "$ASCII_DIR"

# 4. --- FFMPEG: Video -> Image Frames ---
echo "Step 1: Extracting frames from '$VIDEO_FILE'..."
# -i: input file
# -vf: video filter
#   "fps=$TARGET_FPS": extracts X frames per second
#   "scale=$TARGET_WIDTH:-1": resizes to target width, keeping aspect ratio
# "${FRAME_DIR}/frame_%04d.png": output pattern for numbered frames
ffmpeg -i "$VIDEO_FILE" -vf "fps=$TARGET_FPS,scale=$TARGET_WIDTH:-1" "${FRAME_DIR}/frame_%04d.png"

# Check if ffmpeg failed (e.g., no frames extracted)
if [ ! "$(ls -A $FRAME_DIR)" ]; then
    echo "Error: ffmpeg failed to extract any frames."
    rm -rf "$FRAME_DIR" "$ASCII_DIR"
    exit 1
fi

# 5. --- JP2A: Image Frames -> ASCII Files ---
echo "Step 2: Converting $(ls -1 "$FRAME_DIR" | wc -l) frames to ASCII..."
for f in "${FRAME_DIR}"/*.png; do
  # --width=$TARGET_WIDTH: ensures ASCII width matches
  # --output=...: saves to a new .txt file in the ASCII directory
  jp2a --width=$TARGET_WIDTH --output="${ASCII_DIR}/$(basename "$f" .png).txt" "$f"
done

# 6. --- BUNDLING: ASCII Files -> Single Animation File ---
echo "Step 3: Bundling ASCII frames into '$OUTPUT_FILE'..."
# 1. 'find ... -print0': Finds all .txt files in the ascii dir
# 2. 'sort -z': Sorts them alphabetically/numerically (e.g., frame_0001, frame_0002)
# 3. 'xargs -0 -I {} ...': For each file found...
# 4. 'sh -c 'cat {}; ...'': ...print the content of the file ('cat {}'),
# 5. '... echo "$DELIMITER"': ...and then print our delimiter right after it.
# 6. '> "$OUTPUT_FILE"': Redirect all of this into our final output file.
find "$ASCII_DIR" -name "*.txt" -print0 | sort -z | \
  xargs -0 -I {} sh -c 'cat {}; echo "$DELIMITER"' > "$OUTPUT_FILE"

# 7. --- CLEANUP ---
echo "Step 4: Cleaning up temporary directories..."
rm -rf "$FRAME_DIR" "$ASCII_DIR"

echo "Success! Your animation file is ready: $OUTPUT_FILE"