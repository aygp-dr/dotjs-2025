# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run Commands
- Setup repository: `./setup.sh`
- Generate talk templates: `python utils.py templates`
- Add notes: `python utils.py note <topic> <content>`
- Export speaker data: `python utils.py export`

## Development Guidelines
- Use Org Mode for all documentation with the following header structure:
```org
#+TITLE: Document Title
#+DATE: April 3, 2025
#+CATEGORY: dotJS2025
#+PROPERTY: header-args :mkdirp yes
```

## Code Style
- Python: Follow PEP 8 style guidelines
- JavaScript: Use ES6+ syntax in code examples
- Use descriptive variable and function names
- Handle errors with appropriate try/except blocks
- Use docstrings for all functions

## Org Mode Conventions
- Enable Babel for code execution (especially for JavaScript snippets)
- Use `:tangle` property for code export
- Always use `:mkdirp yes` for directory creation
- Create Mermaid diagrams for system/design modeling

## Git Workflow
- Use conventional commit messages: `<type>(<scope>): <description>`
- Format: feat, fix, docs, style, refactor, test, chore
- Always use `--no-gpg-sign` when committing