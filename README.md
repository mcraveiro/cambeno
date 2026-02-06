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
- [llama.cpp](https://github.com/ggerganov/llama.cpp)

### Setting up llama.cpp

1. Download and build `llama.cpp` according to its documentation.
2. Start the server with a compatible model (e.g., GLM-4):
   ```bash
   ./llama-server -hf unsloth/GLM-4-9B-0414-GGUF:Q4_K_M
   ```
   *Note: Ensure the server is running on `http://localhost:8080` (default).*

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

## Development Environment (Emacs)

For the best development experience, it is recommended to use Emacs with either **SLIME** or **Sly**.

1. Install SLIME or Sly via the Emacs package manager (`M-x package-install`).
2. Configure your Lisp implementation in your Emacs init file:
   ```elisp
   (setq inferior-lisp-program "sbcl")
   ```
3. To load Cambeno during development:
   - Symlink the `cambeno` directory into your Quicklisp local projects folder:
     ```bash
     ln -s /path/to/cambeno ~/quicklisp/local-projects/
     ```
   - In Emacs, run `M-x slime` (or `M-x sly`).
   - Load the system:
     ```lisp
     (ql:quickload :cambeno)
     ```

## Usage

### LLM Loop

The core feature is the `run-loop`, which starts an interactive session with a `llama.cpp` server:

```lisp
(cambeno:run-loop "Compute the first 10 prime numbers using Lisp.")
```

This will:
1. Send the prompt to the LLM.
2. If the LLM generates `<lisp>...</lisp>` blocks, they are evaluated.
3. The results are injected back into the context.
4. The process repeats until no more Lisp blocks are found or `max-iterations` is reached.

### Manual Processing

You can process text containing Lisp blocks using the `cambeno:main` function:

```lisp
(cambeno:main "<lisp>(defun hello (name) (format nil \"Hello, ~A!\" name))</lisp> Result: <lisp>(hello \"World\")</lisp>")
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
