#!/bin/bash
# Export Org files to PDF

set -e

echo "Generating PDF documentation..."

# Find all org files and process one at a time
find . -name "*.org" -type f | while read -r file; do
  echo "Exporting $file to PDF..."
  emacs --batch --eval "(require 'org)" "$file" \
    --eval '(org-latex-export-to-pdf)' \
    --kill
done

echo "PDF generation complete."