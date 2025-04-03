#!/bin/bash
# Script to detangle code from source files back to org files

# Find all org files
ORG_FILES=$(find . -name "*.org" -type f)

# Process each org file
for file in $ORG_FILES; do
    echo "Detangling $file..."
    emacs --batch --load "$file" \
        --eval "(require 'org)" \
        --eval "(org-babel-detangle)" \
        --kill
done