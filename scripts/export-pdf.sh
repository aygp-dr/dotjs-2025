#!/bin/bash
# Export Org files to PDF

set -e

echo "Generating PDF documentation..."

# Find all org files
ORG_FILES=$(find . -name "*.org" -type f)

if [ -z "$ORG_FILES" ]; then
  echo "No org files found."
  exit 0
fi

# Export each file to PDF
emacs --batch --eval "(require 'org)" $ORG_FILES \
  --eval '(org-latex-export-to-pdf)' \
  --kill

echo "PDF generation complete."