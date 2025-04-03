#!/bin/bash
# Tangle code from org files to source files

set -e

echo "Tangling code from org files..."

# Find all org files
ORG_FILES=$(find . -name "*.org" -type f)

if [ -z "$ORG_FILES" ]; then
  echo "No org files found."
  exit 0
fi

# Tangle each file
emacs --batch --eval "(require 'org)" $ORG_FILES \
  --eval '(org-babel-tangle)' \
  --kill

echo "Tangling complete."