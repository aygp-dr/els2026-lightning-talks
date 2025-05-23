# Bidirectional Code Generation - Implementation

## Requirements
- SBCL or other Common Lisp implementation
- Quicklisp
- Access to OpenAI or other LLM API

## Setup
1. Clone this repository
2. Load the system with `(ql:quickload :bidirectional-llm)`
3. Configure your API key in `config.lisp`

## Usage
```lisp
(bidirectional-llm:bidirectional-evaluate 
  '(defun fibonacci (n)
     "Calculate the nth Fibonacci number"
     ;; TO-COMPLETE
     ))
```
