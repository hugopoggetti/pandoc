# MyPandoc

## What is Pandoc ?

[Pandoc](https://pandoc.org/) is a universal document converter.
It allows you to convert documents from one format to another (Markdown, HTML, LaTeX, etc.).
It is commonly used to generate professional outputs in formats such as PDF, HTML, or JSON from simple text files.

---

## Project Goal

**MyPandoc** is a custom Pandoc-like converter built as part of a software project. It converts documents between:

- **Markdown**  `input/output`
- **JSON**      `input/output`
- **XML**       `input/output`
- **HTML**      `input/output`

It features:

- A custom parsing library written in Haskell
- A fully defined Abstract Syntax Tree (AST) to represent document structure
- Automatic input format detection based on file extension or content
- Output to a specified file or standard output
- Clear and consistent custom error handling with usage messages and exit codes
---

## Usage

```bash
./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]
```
-  -i : path to input file
-  -f : format of the output
-  -o : path to the output file
-  -e : format of the input file

### Example Commands
```bash
./mypandoc -i file.md -f json -o output.json -e markdown
```

## Documentation – AST and Parsing

This section details how `MyPandoc` internally represents and parses documents.
The entire document is transformed into a custom **Abstract Syntax Tree (AST)** inspired by the structure used in Pandoc,
designed to reflect the semantic structure of content regardless of the input format (Markdown, JSON, or XML).

[Documentation pdf](./doc/doc.pdf) or [markdown](./doc/doc.md)
