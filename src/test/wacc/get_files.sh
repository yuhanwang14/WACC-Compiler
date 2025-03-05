#!/bin/bash

TEST_DIR="./src/test/wacc"
TARGET_FILE_DIR="./src/test/wacc/file_lists"

> "$TARGET_FILE_DIR/valid_files"
find "$TEST_DIR/valid" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_files"

> "$TARGET_FILE_DIR/invalid_files"
find "$TEST_DIR/invalid" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/invalid_files"
