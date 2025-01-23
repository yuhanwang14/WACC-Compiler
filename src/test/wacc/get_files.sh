#!/bin/bash

TEST_DIR="./src/test/wacc"
TARGET_FILE_DIR="./src/test/wacc/file_lists"

> "$TARGET_FILE_DIR/expr_files"
for file in "$TEST_DIR/expr"/*; do
    echo "$file" >> "$TARGET_FILE_DIR/expr_files"
done

> "$TARGET_FILE_DIR/type_files"
for file in "$TEST_DIR/type"/*; do
    echo "$file" >> "$TARGET_FILE_DIR/type_files"
done
