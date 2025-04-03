#!/bin/bash
# Script to tangle or detangle Org files in the project

# Show usage information
show_usage() {
    echo "Usage: $0 [options] [file]"
    echo ""
    echo "Options:"
    echo "  -a, --all       Process all Org files in the project"
    echo "  -d, --detangle  Detangle (sync from source back to Org)"
    echo "  -h, --help      Display this help message"
    echo ""
    echo "Examples:"
    echo "  $0 talks/morning-sessions/ryan-dahl-special-announcement.org  # Tangle a specific file"
    echo "  $0 -d talks/morning-sessions/ryan-dahl-special-announcement.org  # Detangle a specific file"
    echo "  $0 -a  # Tangle all Org files"
    echo "  $0 -ad  # Detangle all Org files"
}

# Process a single file
process_file() {
    local mode=$1
    local file=$2
    
    if [ ! -f "$file" ]; then
        echo "Error: File not found: $file"
        exit 1
    fi
    
    if [ "$mode" == "tangle" ]; then
        emacs --batch --file "$file" --eval "(org-babel-tangle)"
        echo "Tangled: $file"
    else
        emacs --batch --file "$file" --eval "(org-babel-detangle)"
        echo "Detangled: $file"
    fi
}

# Process all files
process_all() {
    local mode=$1
    
    echo "Processing all Org files in the project..."
    
    find . -name "*.org" -type f | while read -r file; do
        if [ "$mode" == "tangle" ]; then
            emacs --batch --file "$file" --eval "(org-babel-tangle)" 2>/dev/null
            echo "Tangled: $file"
        else
            emacs --batch --file "$file" --eval "(org-babel-detangle)" 2>/dev/null
            echo "Detangled: $file"
        fi
    done
    
    echo "Done!"
}

# Parse arguments
ALL_FILES=false
MODE="tangle"

while [[ $# -gt 0 ]]; do
    case $1 in
        -a|--all)
            ALL_FILES=true
            shift
            ;;
        -d|--detangle)
            MODE="detangle"
            shift
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        -*)
            # Handle combined flags like -ad
            if [[ $1 == *"a"* ]]; then
                ALL_FILES=true
            fi
            if [[ $1 == *"d"* ]]; then
                MODE="detangle"
            fi
            if [[ $1 == *"h"* ]]; then
                show_usage
                exit 0
            fi
            shift
            ;;
        *)
            FILE=$1
            shift
            ;;
    esac
done

# Execute the appropriate action
if [ "$ALL_FILES" = true ]; then
    process_all "$MODE"
elif [ -n "$FILE" ]; then
    process_file "$MODE" "$FILE"
else
    echo "Error: No file specified and --all not set."
    show_usage
    exit 1
fi