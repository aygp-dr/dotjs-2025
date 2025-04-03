# dotJS 2025 Conference Notes - Makefile
# Author: Aidan Pace <apace@defrecord.com>

.DEFAULT_GOAL := help

# Helper function for generating help text from comments
help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

# No variables needed as scripts find files themselves

tangle: ## Tangle code blocks from all org files
	@./scripts/tangle.sh

detangle: ## Detangle code back to org files
	@./scripts/detangle.sh

clean: ## Clean generated files (diagrams, tangled code)
	@./scripts/clean.sh

html: ## Generate HTML documentation from org files
	@./scripts/export-html.sh

pdf: ## Generate PDF documentation from org files
	@./scripts/export-pdf.sh

lint: lint-org lint-el lint-py lint-sh ## Run all linters

lint-org: ## Lint org files
	@./scripts/lint-org.sh

lint-el: ## Lint Emacs Lisp files
	@./scripts/lint-elisp.sh

lint-py: ## Lint Python files
	@./scripts/lint-python.sh

lint-sh: ## Lint shell scripts
	@./scripts/lint-shell.sh

.PHONY: help tangle detangle clean html pdf lint lint-org lint-el lint-py lint-sh