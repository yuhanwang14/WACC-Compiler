#!/bin/bash

TEST_DIR="./src/test/wacc"
TARGET_FILE_DIR="./src/test/wacc/file_lists"

> "$TARGET_FILE_DIR/valid_files"
find "$TEST_DIR/valid" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_files"

> "$TARGET_FILE_DIR/invalid_files"
find "$TEST_DIR/invalid" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/invalid_files"

> "$TARGET_FILE_DIR/valid_advanced"
find "$TEST_DIR/valid/advanced" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_advanced"

> "$TARGET_FILE_DIR/valid_array"
find "$TEST_DIR/valid/array" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_array"

> "$TARGET_FILE_DIR/valid_basic"
find "$TEST_DIR/valid/basic" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_basic"

> "$TARGET_FILE_DIR/valid_expressions"
find "$TEST_DIR/valid/expressions" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_expressions"

> "$TARGET_FILE_DIR/valid_function"
find "$TEST_DIR/valid/advanced" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_function"

> "$TARGET_FILE_DIR/valid_if"
find "$TEST_DIR/valid/if" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_if"

> "$TARGET_FILE_DIR/valid_IO"
find "$TEST_DIR/valid/IO" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_IO"

> "$TARGET_FILE_DIR/valid_pairs"
find "$TEST_DIR/valid/pairs" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_pairs"

> "$TARGET_FILE_DIR/valid_runtimeErr"
find "$TEST_DIR/valid/runtimeErr" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_runtimeErr"

> "$TARGET_FILE_DIR/valid_scope"
find "$TEST_DIR/valid/scope" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_scope"

> "$TARGET_FILE_DIR/valid_sequence"
find "$TEST_DIR/valid/sequence" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_sequence"

> "$TARGET_FILE_DIR/valid_variables"
find "$TEST_DIR/valid/variables" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_variables"

> "$TARGET_FILE_DIR/valid_while"
find "$TEST_DIR/valid/while" -type f -name '*.wacc' >> "$TARGET_FILE_DIR/valid_while"
