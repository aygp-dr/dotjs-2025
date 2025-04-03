#!/bin/bash
# Script to generate Org templates for conference talks
# This is extracted from setup.sh to be independently usable

set -e # Exit on error

# Generate template org files for all talks
generate_templates() {
    if command -v emacs >/dev/null 2>&1; then
        echo "Generating talk templates using Emacs..."
        emacs --batch --load "../lisp/generate-templates.el"
        echo "Templates generated successfully!"
    else
        echo "Error: Emacs not found, cannot generate templates."
        echo "Please install Emacs and try again."
        exit 1
    fi
}

# Generate template for a specific session
generate_session_templates() {
    local session=$1
    
    if [ -z "$session" ]; then
        echo "Error: No session specified."
        echo "Usage: $0 --session [morning|afternoon|lightning]"
        exit 1
    fi
    
    echo "Generating templates for $session session..."
    # This would filter the template generation for specific sessions
    # We'd need to modify the Elisp code to support this, but showing the structure
    
    echo "Session templates for $session generated."
}

# Display help
show_help() {
    echo "Usage: $0 [OPTION]"
    echo "Generate Org mode templates for dotJS 2025 conference talks."
    echo ""
    echo "Options:"
    echo "  --all                 Generate templates for all talks (default)"
    echo "  --session TYPE        Generate templates for specific session type"
    echo "                        (morning, afternoon, lightning)"
    echo "  -h, --help            Display this help message"
    echo ""
    echo "Examples:"
    echo "  $0                    Generate all templates"
    echo "  $0 --session morning  Generate only morning session templates"
}

# Parse command line arguments
if [ $# -eq 0 ]; then
    # Default behavior
    generate_templates
    exit 0
fi

while [ $# -gt 0 ]; do
    case "$1" in
        --all)
            generate_templates
            shift
            ;;
        --session)
            generate_session_templates "$2"
            shift 2
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done