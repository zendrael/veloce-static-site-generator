#!/bin/sh

# Veloce builder


# Define source directory and output directory
SRC_DIR="src"
BUILD_DIR="build"
OUTPUT_FILE="$BUILD_DIR/veloce"

# Create build directory if it doesn't exist
mkdir -p "$BUILD_DIR"

# Compile the Pascal project
fpc -Os -CX -O3 -Ooregvar -Xg -Xs -XX -Fi"$SRC_DIR" -FU"$BUILD_DIR" -FE"$BUILD_DIR" -o"$OUTPUT_FILE" "$SRC_DIR/veloce.pas"

# Check if the compilation was successful
if [ $? -eq 0 ]; then
  echo "Compilation successful. Executable created at $OUTPUT_FILE"

  cd $BUILD_DIR
  find ./ -name \*.a | xargs rm -f
  find ./ -name \*.o | xargs rm -f
  find ./ -name \*.ppu | xargs rm -f
  find ./ -name \*.or | xargs rm -f
  find ./ -name \*.compiled | xargs rm -f
  find ./ -name \*.tmp | xargs rm -f
  find ./ -name \*.dbg | xargs rm -f
else
  echo "Compilation failed."
  exit 1
fi

