#!/bin/bash
# Lint shell scripts using shellcheck

set -e

echo "Linting shell scripts..."

# Find shell scripts
SH_FILES=$(find ./scripts -name "*.sh" -type f 2>/dev/null)

if [ -z "$SH_FILES" ]; then
  echo "No shell scripts found."
  exit 0
fi

# Check if shellcheck is available
if command -v shellcheck >/dev/null; then
  find ./scripts -name "*.sh" -type f -exec shellcheck {} \;
else
  echo "shellcheck not found, skipping shell linting"
  exit 1
fi

echo "Shell linting complete."