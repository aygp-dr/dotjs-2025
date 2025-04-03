#!/bin/bash
# Export Org files to HTML

set -e

echo "Generating HTML documentation..."

# Find all org files and process one at a time
find . -name "*.org" -type f | while read -r file; do
  echo "Exporting $file to HTML..."
  emacs --batch --eval "(require 'org)" "$file" \
    --eval '(org-html-export-to-html)' \
    --kill
done

echo "HTML generation complete."