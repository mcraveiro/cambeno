# Cambeno

Cambeno is an implementation of the architecture proposed in the research paper [**"From Tool Calling to Symbolic Thinking: LLMs in a Persistent Lisp Metaprogramming Loop"**](https://arxiv.org/html/2506.10021v1).

The project aims to provide LLMs with a persistent, stateful Lisp environment where they can dynamically define, evolve, and invoke their own tools.

## Key Features

- **Persistent Lisp REPL**: State, function definitions, and variables persist across interactions.
- **Middleware Layer**: Automatically extracts and evaluates `<lisp>...</lisp>` tags within LLM-generated text.
- **Self-Inspection Utilities**: Built-in functions that allow the LLM to inspect its own environment and definitions.
- **Symbolic Reasoning**: Leverages Common Lisp's power for dynamic metaprogramming and DSL creation.

## Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/beta/) (for managing dependencies)

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/cambeno.git
   cd cambeno
   ```

2. Load the system in SBCL:
   ```lisp
   (push #p"/path/to/cambeno/" asdf:*central-registry*)
   (asdf:load-system :cambeno)
   ```

## Usage

You can process text containing Lisp blocks using the `cambeno:main` function:

```lisp
(cambeno:main "<lisp>(defun hello (name) (format nil "Hello, ~A!" name))</lisp> Result: <lisp>(hello "World")</lisp>")
```

### Self-Inspection

The LLM can use the `cambeno.utils` package to explore the environment:

```lisp
;; List exported functions in a package
(cambeno.utils:list-functions :cambeno.repl)

;; Inspect a specific symbol
(cambeno.utils:inspect-symbol 'cambeno.repl:eval-lisp-string)
```

## Running Tests

Tests are implemented using [FiveAM](https://github.com/sionescu/fiveam). Run them via ASDF:

```lisp
(asdf:test-system :cambeno)
```

## License

MIT
