# dotJS 2025 Conference Notes - Makefile
# Author: Aidan Pace <apace@defrecord.com>

.DEFAULT_GOAL := help

# Helper function for generating help text from comments
help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

# Find all org files
ORG_FILES := $(shell find . -name "*.org" -type f)

tangle: ## Tangle code blocks from all org files
	@echo "Tangling code from org files..."
	@emacs --batch --eval "(require 'org)" $(ORG_FILES) \
		--eval '(org-babel-tangle)' \
		--kill
	@echo "Tangling complete."

detangle: ## Detangle code back to org files
	@echo "Detangling code back to org files..."
	@./scripts/detangle.sh
	@echo "Detangling complete."

clean: ## Clean generated files (diagrams, tangled code)
	@echo "Cleaning generated files..."
	@find ./code-examples -name "*.js" -type f -delete
	@find ./diagrams -name "*.svg" -type f -delete
	@find ./diagrams -name "*.png" -type f -delete
	@echo "Cleaning complete."

html: ## Generate HTML documentation from org files
	@echo "Generating HTML documentation..."
	@emacs --batch --eval "(require 'org)" $(ORG_FILES) \
		--eval '(org-html-export-to-html)' \
		--kill
	@echo "HTML generation complete."

pdf: ## Generate PDF documentation from org files
	@echo "Generating PDF documentation..."
	@emacs --batch --eval "(require 'org)" $(ORG_FILES) \
		--eval '(org-latex-export-to-pdf)' \
		--kill
	@echo "PDF generation complete."

lint-org: ## Lint org files
	@echo "Linting org files..."
	@emacs --batch --eval "(require 'org-lint)" $(ORG_FILES) \
		--eval '(org-lint)' \
		--kill
	@echo "Linting complete."

.PHONY: help tangle detangle clean html pdf lint-org