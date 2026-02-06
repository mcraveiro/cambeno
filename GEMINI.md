# Cambeno: From Tool Calling to Symbolic Thinking

This project implements the architecture proposed in the paper: **"From Tool Calling to Symbolic Thinking: LLMs in a Persistent Lisp Metaprogramming Loop"** ([arXiv:2506.10021v1](https://arxiv.org/html/2506.10021v1)).

## Core Concepts

The goal is to transition from static tool-calling to a dynamic, symbolic thinking process by embedding an LLM within a persistent Lisp metaprogramming loop.

### 1. Architecture
- **LLM Backend**: Generates text and embedded Lisp code blocks (e.g., inside `<lisp>...</lisp>` tags).
- **Middleware**: Intercepts these tags, sends the code to a Lisp REPL, and injects the evaluation results back into the LLM's context in real-time.
- **Persistent Lisp REPL**: A long-running Common Lisp process (e.g., SBCL) that maintains state, definitions, and history across multiple turns.

### 2. Key Capabilities
- **Stateful Tool Construction**: The LLM can define new functions and variables that persist, building its own library of utilities.
- **Reflection & Self-Correction**: The LLM can inspect existing Lisp code, documentation, and state to debug or optimize its own logic.
- **Metaprogramming**: Leveraging Lisp macros to create Domain Specific Languages (DSLs) for complex reasoning tasks.
- **Generative Self-Extension**: The LLM can potentially modify the very routines it uses to process information.

## Implementation Roadmap

1.  **Environment Setup**: Establish a persistent Common Lisp (SBCL) process.
2.  **Middleware Development**: Build a proxy layer to handle the streaming extraction and execution of `<lisp>` blocks.
3.  **Communication Protocol**: Define how the LLM receives REPL output (stdout, stderr, and return values).
4.  **Sandboxing**: Ensure safe execution of generated code through OS-level or Lisp-level restrictions.
5.  **Integration**: Hook the system into a chat interface or autonomous agent loop.

## Usage

The system is currently structured as a Common Lisp project.

### Loading the system:
```lisp
(asdf:load-system :cambeno)
```

### Processing LLM output:
```lisp
(cambeno:main "<lisp>(defun square (x) (* x x))</lisp> The square of 5 is <lisp>(square 5)</lisp>")
```

### Self-Inspection:
The LLM can use `cambeno.utils` functions to inspect the environment:
- `(cambeno.utils:list-functions :package-name)`: List all exported functions.
- `(cambeno.utils:inspect-symbol 'symbol-name)`: Get detailed description of a symbol.

## Current Progress
- [x] ASDF system structure
- [x] Basic REPL with output capture
- [x] Middleware for tag extraction and result injection
- [x] Self-inspection utilities
- [x] llama.cpp HTTP API integration
- [x] Persistent loop implementation (`run-loop`)
- [ ] Advanced Sandboxing
- [ ] Streaming Middleware implementation
