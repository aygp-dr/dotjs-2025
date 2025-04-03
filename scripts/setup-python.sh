#!/bin/bash
# Set up Python environment with Poetry or pip

set -e

echo "Setting up Python environment..."

# Check if Poetry is available
if command -v poetry >/dev/null; then
  echo "Using Poetry for dependency management"
  poetry install --no-root
elif command -v pip >/dev/null; then
  echo "Using pip for dependency management"
  pip install -r requirements.txt
else
  echo "Error: Neither Poetry nor pip found. Please install one of them."
  exit 1
fi

echo "Python setup complete."