#!/bin/bash

# Create main directory structure
mkdir -p common/{assets,code,references}
mkdir -p presentations/{01-llm-lisp-bidirectional,02-repl-observability,03-incremental-types,04-lisp-web-components,05-protocol-programming}
mkdir -p planning/mermaid
mkdir -p docs

# Set up presentation subdirectories
for dir in presentations/*/; do
  mkdir -p "${dir}code" "${dir}notes" "${dir}slides" "${dir}demo"
  
  # Create README for each presentation
  cat > "${dir}README.md" << EOF
# $(basename "${dir}" | sed 's/^[0-9]*-//g' | tr '-' ' ' | sed -e 's/\b\(.\)/\u\1/g')

## Concept
Brief description of the concept goes here.

## Implementation Goals
- Goal 1
- Goal 2
- Goal 3

## Demo Ideas
- Demo idea 1
- Demo idea 2

## Resources
- [Resource 1](URL)
- [Resource 2](URL)
EOF
done

# Create main README with tracking table
cat > README.md << EOF
# ELS 2026 Lightning Talk Development

Repository for developing potential lightning talks for the European Lisp Symposium 2026.

## Progress Tracking

| Talk | Concept | Code | Demo | Slides | Rehearsed | Notes |
|------|---------|------|------|--------|-----------|-------|
| Bidirectional Code Gen | 10% | 0% | 0% | 0% | 0% | Initial research phase |
| REPL Observability | 10% | 0% | 0% | 0% | 0% | Initial research phase |
| Incremental Types | 10% | 0% | 0% | 0% | 0% | Initial research phase |
| Lisp Web Components | 10% | 0% | 0% | 0% | 0% | Initial research phase |
| Protocol Programming | 10% | 0% | 0% | 0% | 0% | Initial research phase |

## Repository Structure

\`\`\`
els2026-lightning-talks/
├── common/            # Shared resources
├── presentations/     # Individual presentation folders
├── planning/          # Project planning documents
└── docs/              # Documentation
\`\`\`

## Timeline

- **June 2025**: Initial research and concept development
- **September 2025**: First code prototypes
- **December 2025**: Narrow down to 2-3 strongest candidates
- **February 2026**: Final selection and polish
- **April 2026**: Submit final proposal(s) to ELS 2026
EOF

# Create org-mode planning files
cat > planning/journal.org << EOF
#+TITLE: ELS 2026 Development Journal
#+AUTHOR: Your Name
#+DATE: $(date +%Y-%m-%d)

* 2025-05-21 Project Initialization
- Set up repository structure
- Outlined initial concepts for all five presentations

* TODO Next Steps
- Research previous ELS lightning talks on similar topics
- Investigate current state of Lisp/LLM integration
- Explore libraries for REPL observability
EOF

cat > planning/timeline.org << EOF
#+TITLE: ELS 2026 Lightning Talk Development Timeline
#+AUTHOR: Your Name

* Phase 1: Exploration [2025-05 to 2025-08]
** TODO Initial research for all five concepts
** TODO Create proof of concept code for most promising 2-3 ideas
** TODO Evaluate technical feasibility

* Phase 2: Development [2025-09 to 2025-12]
** TODO Develop functional prototypes for top 3 candidates
** TODO Create initial demo scripts
** TODO Gather peer feedback

* Phase 3: Selection [2026-01 to 2026-02]
** TODO Evaluate all candidates against selection criteria
** TODO Select final presentation(s)
** TODO Plan final implementation details

* Phase 4: Refinement [2026-02 to 2026-04]
** TODO Complete implementation and polish demo
** TODO Create and refine slides
** TODO Rehearse presentation
** TODO Submit proposal to ELS 2026
EOF

cat > planning/evaluation.org << EOF
#+TITLE: Evaluation Criteria for ELS 2026 Presentations
#+AUTHOR: Your Name

* Selection Criteria
| Criterion | Weight | Description |
|-----------|--------|-------------|
| Novelty | 5 | How original is the idea in the Lisp community? |
| Impact | 4 | How useful would this be to Lisp practitioners? |
| Demo Quality | 5 | How impressive and illustrative is the demo? |
| Implementation Completeness | 3 | How fully implemented is the concept? |
| Presentation Clarity | 4 | How clearly can the idea be communicated in 5 minutes? |

* Evaluation Rubric
** 5: Exceptional - Groundbreaking, complete, impressive
** 4: Very Good - Novel, mostly complete, clear
** 3: Good - Interesting, functional, understandable
** 2: Fair - Derivative, partially implemented, somewhat unclear
** 1: Poor - Common, barely implemented, confusing

* Presentation Evaluation Template

** Bidirectional Code Generation
- Novelty: [Score]
- Impact: [Score]
- Demo Quality: [Score]
- Implementation Completeness: [Score]
- Presentation Clarity: [Score]
- Total Weighted Score: [Calculated]
- Notes:

[Repeat for other presentations]
EOF

# Create an example Mermaid diagram
cat > planning/mermaid/bidirectional-flow.md << EOF
# Bidirectional Code Generation Flow

\`\`\`mermaid
graph TD
    A[Lisp REPL] -->|Generates prompts| B[LLM API]
    B -->|Returns completions| C[Parser]
    C -->|Structured code| D[Evaluator]
    D -->|Execution results| A
    
    E[User Input] -->|Code modifications| A
    A -->|Current state| E
    
    F[Knowledge Base] -->|Context| B
    D -->|Updates| F
\`\`\`
EOF

# Create a template presentation file
cat > presentations/01-llm-lisp-bidirectional/notes/presentation.org << EOF
#+TITLE: Bidirectional Code Generation with Lisp and LLMs
#+AUTHOR: Your Name
#+DATE: ELS 2026

* Concept Overview
  :PROPERTIES:
  :PROGRESS: [0/5]
  :END:
  - [ ] Define key terms and concepts
  - [ ] Clarify the innovation
  - [ ] Identify limitations in current approaches
  - [ ] Explain benefits of bidirectional approach
  - [ ] Outline potential applications
  
* Implementation Plan
  :PROPERTIES:
  :PROGRESS: [0/4]
  :END:
  - [ ] Core API design
  - [ ] LLM integration
  - [ ] Parser development
  - [ ] Evaluation component
  
* Demo Script
  :PROPERTIES:
  :PROGRESS: [0/3]
  :END:
  - [ ] Initial setup
  - [ ] Key interaction points
  - [ ] Wow moment
  
* Slide Outline
  :PROPERTIES:
  :PROGRESS: [0/5]
  :END:
  - [ ] Introduction
  - [ ] Problem statement
  - [ ] Technical approach
  - [ ] Demo
  - [ ] Conclusion
  
* Key Points to Convey
  1. Lisp's symbolic expressions are ideal for AI communication
  2. Bidirectional flow enables self-improving systems
  3. Real-world applications
  
* References & Resources

* Feedback & Iterations
EOF

# Create a .gitignore file
cat > .gitignore << EOF
# Emacs backup files
*~
\#*\#
.\#*

# Common Lisp implementation files
*.fasl
*.dx64fsl
*.dx32fsl
*.lx64fsl
*.lx32fsl
*.x86f
*.sparcf

# Build directories
/build/
/dist/

# Generated files
*.pdf
*.html
!docs/*.html

# OS specific files
.DS_Store
Thumbs.db

# Editor directories
.vscode/
.idea/

# Log files
*.log
EOF

# Create a minimal sample code file
mkdir -p presentations/01-llm-lisp-bidirectional/code/src
cat > presentations/01-llm-lisp-bidirectional/code/src/package.lisp << EOF
(defpackage :bidirectional-llm
  (:use :cl)
  (:export :generate-from-code
           :code-from-generation
           :bidirectional-evaluate))

(in-package :bidirectional-llm)

;; TODO: Implement core functionality
EOF

cat > presentations/01-llm-lisp-bidirectional/code/README.md << EOF
# Bidirectional Code Generation - Implementation

## Requirements
- SBCL or other Common Lisp implementation
- Quicklisp
- Access to OpenAI or other LLM API

## Setup
1. Clone this repository
2. Load the system with \`(ql:quickload :bidirectional-llm)\`
3. Configure your API key in \`config.lisp\`

## Usage
\`\`\`lisp
(bidirectional-llm:bidirectional-evaluate 
  '(defun fibonacci (n)
     "Calculate the nth Fibonacci number"
     ;; TO-COMPLETE
     ))
\`\`\`
EOF

# Initialize git repository
git init
git add .
git commit -m "Initial repository setup for ELS 2026 lightning talks"

echo "Repository structure created successfully!"
echo "Next steps:"
echo "1. Review the file structure and templates"
echo "2. Begin filling in research notes for each concept"
echo "3. Start exploration phase according to planning/timeline.org"
