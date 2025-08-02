# Molt

Molt is a syntax-aware search and replace tool for Rust. Molt enables automatic refactoring using a compact and powerful language.

## Overview

Molt provides a domain-specific language (DSL) for defining code modifications and pattern matching on the Rust Abstract Syntax Tree (AST). It's particularly useful for:

- **Code refactoring**: Automatically modify code patterns across your codebase
- **API migrations**: Update function calls when APIs change
- **Pattern finding**: Search for specific code patterns in your codebase

## Attribution

The Rust parser for this project is based on [syn](https://github.com/dtolnay/syn).
