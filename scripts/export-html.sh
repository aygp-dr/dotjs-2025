#!/bin/bash
# Export Org files to HTML

set -e

echo "Generating HTML documentation..."

# Find all org files
ORG_FILES=$(find . -name "*.org" -type f)

if [ -z "$ORG_FILES" ]; then
  echo "No org files found."
  exit 0
fi

# Export each file to HTML
emacs --batch --eval "(require 'org)" $ORG_FILES \
  --eval '(org-html-export-to-html)' \
  --kill

echo "HTML generation complete."