#!/bin/bash
# Clean generated files

set -e

echo "Cleaning generated files..."

# Clean JavaScript files
find ./code-examples -name "*.js" -type f -delete

# Clean diagrams
find ./diagrams -name "*.svg" -type f -delete
find ./diagrams -name "*.png" -type f -delete

echo "Cleaning complete."