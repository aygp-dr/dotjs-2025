#!/bin/bash
# Lint Org-mode files

set -e

echo "Linting org files..."

# Find all org files
ORG_FILES=$(find . -name "*.org" -type f)

if [ -z "$ORG_FILES" ]; then
  echo "No org files found."
  exit 0
fi

# Run org-lint on each file
emacs --batch \
  --eval "(require 'org)" \
  --eval '(condition-case nil 
            (progn 
              (require '\''org-lint) 
              (dolist (file command-line-args-left) 
                (with-current-buffer (find-file-noselect file) 
                  (org-lint)))) 
            (error (message "org-lint not available")))' \
  $ORG_FILES \
  --kill

echo "Org linting complete."