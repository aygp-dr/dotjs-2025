#!/bin/bash
# Lint Emacs Lisp files using checkdoc and byte-compilation

set -e

echo "Linting Emacs Lisp files..."

# Find all Elisp files and process one at a time
find lisp -name "*.el" -type f | while read -r file; do
  echo "Linting $file..."
  emacs --batch \
    --eval "(setq byte-compile-error-on-warn t)" \
    --eval "(require 'checkdoc)" \
    --eval '(let ((file "'"$file"'"))
             (with-current-buffer (find-file-noselect file)
               (checkdoc-current-buffer t)
               (byte-compile-file file)))'
done

echo "Emacs Lisp linting complete."