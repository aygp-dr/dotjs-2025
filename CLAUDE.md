# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run Commands
- Always use `make help` to discover available commands
- Prefer Makefile targets over direct script execution
- Python dependency management: `make setup-python`
- Linting: `make lint`
- Code extraction: `make tangle` and `make detangle`

## Documentation Guidelines
- :important: Always create proper file links using `[[file:path][text]]` syntax
- :important: Never make the reader navigate manually to find files
- :important: Never duplicate Makefile targets in documentation
- :important: Use Org's tilde syntax (`~code~`) not backticks for inline code
- Don't include cloning instructions in README (redundant)
- Keep README concise and focused on essential information
- Always verify links work before committing

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

## Makefile Guidelines
- :important: Keep targets under 5 lines
- :important: Move conditional logic and loops to scripts
- :important: Target names should express roles not implementation
- Make targets should be idempotent (safe to run multiple times)

## Development Workflow
- :important: Always test changes immediately after making them
- :important: Run linting before committing any changes
- :important: Verify org-mode files after editing (links, syntax, etc.)

## Git Workflow
- Use conventional commit messages: `<type>(<scope>): <description>`
- Format: feat, fix, docs, style, refactor, test, chore
- Always use `--no-gpg-sign` when committing
- Use trailers for co-authors and reviewers