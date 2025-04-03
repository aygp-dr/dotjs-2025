#!/bin/bash
# Lint Org-mode files

set -e

echo "Linting org files..."

# Find all org files and process one at a time
find . -name "*.org" -type f | while read -r file; do
  echo "Linting $file..."
  emacs --batch \
    --eval "(require 'org)" \
    --eval '(condition-case nil 
              (progn 
                (require '\''org-lint) 
                (with-current-buffer (find-file-noselect "'"$file"'") 
                  (org-lint))) 
              (error (message "org-lint not available")))' \
    --kill
done

echo "Org linting complete."