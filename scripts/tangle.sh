#!/bin/bash
# Tangle code from org files to source files

set -e

echo "Tangling code from org files..."

# Find all org files and process one at a time
find . -name "*.org" -type f | while read -r file; do
  echo "Tangling $file..."
  emacs --batch --eval "(require 'org)" "$file" \
    --eval '(org-babel-tangle)' \
    --kill
done

echo "Tangling complete."