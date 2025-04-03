#!/bin/bash
# Lint Python files using ruff

set -e

echo "Linting Python files..."

# Find Python files
PY_FILES=$(find . -name "*.py" -type f)

if [ -z "$PY_FILES" ]; then
  echo "No Python files found."
  exit 0
fi

# Check if poetry is available
if command -v poetry >/dev/null; then
  poetry run ruff check .
elif command -v ruff >/dev/null; then
  ruff check .
else
  echo "ruff not found, skipping Python linting"
  exit 1
fi

echo "Python linting complete."