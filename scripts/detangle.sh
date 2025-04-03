#!/bin/bash
# Detangle code from org files back to source files

set -e

echo "Detangling code back to org files..."

# Find all org files
ORG_FILES=$(find . -name "*.org" -type f)

if [ -z "$ORG_FILES" ]; then
  echo "No org files found."
  exit 0
fi

# Detangle each file
for file in $ORG_FILES; do
  echo "Detangling $file..."
  emacs --batch --file "$file" \
    --eval "(require 'org)" \
    --eval "(condition-case nil (org-babel-detangle) (error (message \"Could not detangle %s\" \"$file\")))" \
    --kill
done

echo "Detangling complete."