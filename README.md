# Scheme

![Build Status](https://github.com/prefics/scheme/workflows/basic-build/badge.svg)

This repository contains a Scheme implementation that follows the R5RS
Standard. It supports different deployments strategies:

* Interactive. A standard REPL is accessible for interactive use.

* Image base. The contents of the heap ca be saved to a file and the
  session can be resumed at a later point.

* Executable. Scheme code can be compiled and an binary executable
  file can be produced.

* Scripts. Simple script files can be executed for rapid scripting

# Installation

Use the classic dance within a shell

```bash
> ./configure
> make
> make install
```

# Usage

## REPL

Run the command `scmrepl` to start the REPL (Read Eval Print
Loop). You enter your definitions at the prompt and they are directly
evaluated.

The REPL supports commands. Commands are entered by prefixing the
command name with a comma (,). The `load` command, to load definitions
from a file, should be entered with `,load`.

# Scheme Extensions

This implementation supports the following Scheme extensions:

* Module system.

* Threads (green threads).

* Dylan like Object System. 

* It supports many SCSH procedures for interacting with the OS.

* Exception system based on Dylan conditons.

# License

This program is distributed under the MIT License.
