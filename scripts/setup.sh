#!/bin/bash
# Setup script for dotJS 2025 conference notes project

set -e # Exit on error

# Create directory structure
create_dirs() {
    echo "Creating directory structure..."
    
    dirs=(
        "talks/morning-sessions"
        "talks/afternoon-sessions"
        "resources/slides"
        "resources/references"
        "resources/bibliography"
        "code-examples/demos"
        "code-examples/experiments"
        "notes/keynotes"
        "notes/lightning-talks"
        "notes/qa-sessions"
        "diagrams"
    )
    
    for dir in "${dirs[@]}"; do
        mkdir -p "$dir"
        echo "  Created $dir"
    done
    
    echo "Directory structure created successfully!"
}

# Generate template org files for all talks
generate_templates() {
    if command -v emacs >/dev/null 2>&1; then
        echo "Generating talk templates using Emacs..."
        emacs --batch --load "./lisp/generate-templates.el"
    else
        echo "Emacs not found, skipping template generation."
        echo "You can manually generate templates later using: emacs --batch --load './lisp/generate-templates.el'"
    fi
}

# Create bibliography file if it doesn't exist
create_bibliography() {
    local bib_file="resources/bibliography/references.bib"
    
    if [ -f "$bib_file" ]; then
        echo "Bibliography file already exists at $bib_file (skipping)"
    else
        echo "Creating bibliography file..."
        cat > "$bib_file" << EOF
@book{crockford2008javascript,
  title={JavaScript: The Good Parts},
  author={Crockford, Douglas},
  year={2008},
  publisher={O'Reilly Media, Inc.}
}

@book{simpson2015you,
  title={You Don't Know JS: ES6 \& Beyond},
  author={Simpson, Kyle},
  year={2015},
  publisher={O'Reilly Media, Inc.}
}

@online{WebAIMDI,
  author = {WebAIM},
  title = {Web Accessibility for Developers with ADHD},
  year = {2023},
  url = {https://webaim.org/articles/adhd/},
  urldate = {2025-04-01}
}
EOF
        echo "Bibliography file created at $bib_file"
    fi
}

# Main execution
echo "Setting up dotJS 2025 conference notes project..."

create_dirs
generate_templates
create_bibliography

echo "Setup complete! You can now start taking notes for the conference."
echo ""
echo "Quick usage guide:"
echo "1. Edit talk notes in the talks/ directory"
echo "2. Run code blocks with C-c C-c in Emacs"
echo "3. Tangle code with C-c C-v t in Emacs"
echo "4. Export HTML with C-c C-e h h in Emacs"
echo ""
echo "For more advanced usage, load the Elisp environment in Emacs:"
echo "  (load-file \"~/projects/aygp-dr/dotjs-2025/lisp/dotjs-init.el\")"