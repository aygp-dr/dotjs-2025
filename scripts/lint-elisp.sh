#!/bin/bash
# Lint Emacs Lisp files using checkdoc and byte-compilation

set -e

echo "Linting Emacs Lisp files..."

# Find all Elisp files (relative paths)
EL_FILES=$(find lisp -name "*.el" -type f)

if [ -z "$EL_FILES" ]; then
  echo "No Elisp files found."
  exit 0
fi

# Run checkdoc and byte-compile on each file
emacs --batch \
  --eval "(setq byte-compile-error-on-warn t)" \
  --eval "(require 'checkdoc)" \
  --eval "(dolist (file command-line-args-left) 
           (with-current-buffer (find-file-noselect file) 
           (checkdoc-current-buffer t) 
           (byte-compile-file file)))" \
  $EL_FILES

echo "Emacs Lisp linting complete."