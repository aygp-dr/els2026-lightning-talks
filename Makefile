# ELS 2026 Lightning Talk Development Makefile
.PHONY: help setup clean build test docs presentations status

# Default target
help:
	@echo "ELS 2026 Lightning Talk Development"
	@echo ""
	@echo "Available targets:"
	@echo "  setup         - Initialize repository structure"
	@echo "  clean         - Remove generated files"
	@echo "  build         - Build all code examples"
	@echo "  test          - Run tests for all presentations"
	@echo "  docs          - Generate documentation"
	@echo "  presentations - Build all presentation materials"
	@echo "  status        - Show development progress"
	@echo "  lisp-build    - Build Common Lisp components"
	@echo "  racket-build  - Build Racket components"
	@echo "  clojure-build - Build Clojure components"

# Initialize repository structure using the setup script
setup:
	@echo "Setting up repository structure..."
	@chmod +x scripts/setup_repo.sh
	@./scripts/setup_repo.sh

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.fasl" -delete
	@find . -name "*.dx64fsl" -delete
	@find . -name "*.lx64fsl" -delete
	@find . -name "*.pdf" -delete
	@find . -name "*.html" \! -path "./docs/*" -delete
	@rm -rf build/
	@rm -rf dist/

# Build all code examples
build: lisp-build racket-build clojure-build
	@echo "Building all code examples..."

# Build Common Lisp components
lisp-build:
	@echo "Building Common Lisp components..."
	@for dir in presentations/*/code/; do \
		if [ -f "$$dir/src/package.lisp" ]; then \
			echo "Building $$dir"; \
			cd "$$dir" && sbcl --eval '(ql:quickload :system)' --quit 2>/dev/null || true; \
			cd - > /dev/null; \
		fi \
	done

# Build Racket components
racket-build:
	@echo "Building Racket components..."
	@for dir in talk*-racket*/; do \
		if [ -d "$$dir" ] && [ -f "$$dir/main.rkt" ]; then \
			echo "Building $$dir"; \
			cd "$$dir" && raco make main.rkt || true; \
			cd - > /dev/null; \
		fi \
	done

# Build Clojure components
clojure-build:
	@echo "Building Clojure components..."
	@for dir in talk*-clojure*/; do \
		if [ -d "$$dir" ] && [ -f "$$dir/project.clj" ]; then \
			echo "Building $$dir"; \
			cd "$$dir" && lein compile || true; \
			cd - > /dev/null; \
		fi \
	done

# Run tests
test:
	@echo "Running tests for all presentations..."
	@for dir in presentations/*/code/; do \
		if [ -f "$$dir/test.lisp" ]; then \
			echo "Testing $$dir"; \
			cd "$$dir" && sbcl --eval '(ql:quickload :system)' --eval '(load "test.lisp")' --quit 2>/dev/null || true; \
			cd - > /dev/null; \
		fi \
	done

# Generate documentation from org files
docs:
	@echo "Generating documentation..."
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(setq org-export-with-toc nil)" \
		--eval "(setq org-html-head-include-default-style nil)" \
		README.org \
		--funcall org-html-export-to-html
	@mv README.html docs/ 2>/dev/null || true
	@for file in planning/*.org; do \
		emacs --batch \
			--eval "(require 'org)" \
			--eval "(setq org-export-with-toc nil)" \
			"$$file" \
			--funcall org-html-export-to-html; \
		mv "$${file%.org}.html" docs/ 2>/dev/null || true; \
	done

# Build presentation materials
presentations:
	@echo "Building presentation materials..."
	@for dir in presentations/*/; do \
		if [ -f "$$dir/slides/slides.org" ]; then \
			echo "Building slides for $$dir"; \
			cd "$$dir/slides" && \
			emacs --batch \
				--eval "(require 'org)" \
				--eval "(require 'ox-beamer)" \
				slides.org \
				--funcall org-beamer-export-to-pdf; \
			cd - > /dev/null; \
		fi \
	done

# Show development progress
status:
	@echo "ELS 2026 Lightning Talk Development Status"
	@echo "========================================"
	@echo ""
	@echo "Repository Structure:"
	@find presentations/ -maxdepth 2 -type d  < /dev/null |  sort
	@echo ""
	@echo "Code Files:"
	@find presentations/ -name "*.lisp" -o -name "*.rkt" -o -name "*.clj" | wc -l | xargs echo "  Total files:"
	@echo ""
	@echo "Documentation Files:"
	@find . -name "*.org" -o -name "README.md" | wc -l | xargs echo "  Total files:"
	@echo ""
	@echo "Recent Git Activity:"
	@git log --oneline -5 2>/dev/null || echo "  No git history"
