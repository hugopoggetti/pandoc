# MyPandoc

## What is Pandoc ?

[Pandoc](https://pandoc.org/) is a universal document converter.
It allows you to convert documents from one format to another (Markdown, HTML, LaTeX, etc.).
It is commonly used to generate professional outputs in formats such as PDF, HTML, or JSON from simple text files.

---

## Project Goal

**MyPandoc** is a custom Pandoc-like converter built as part of a software project. It converts documents between:

- **Markdown**
- **JSON**
- **XML**

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

---

## 🌳 Abstract Syntax Tree (AST)

At the core of the system lies the `Document` type, which encapsulates both metadata and content blocks.

A Document consists of:
- `Meta`: Information about the document (title, author(s), date)
- `[Block]`: A list of content blocks (headings, paragraphs, lists, etc.)
```haskell
-- | Represents a complete document with metadata and content blocks.
data Document = Document Meta [Block]
  deriving (Show, Eq)
```

### Metadata (Header)

The `Meta` type describes the document's metadata fields.

```haskell
-- | Metadata of the document: title, authors, date, etc.
data Meta = Meta
  { metaTitle    :: [Inline]                   -- ^ Document title
  , metaAuthors  :: [[Inline]]                 -- ^ List of authors (each a list of inline elements)
  , metaDate     :: [Inline]                   -- ^ Date of the document
  } deriving (Show, Eq)
```
This flexible structure allows inline formatting even within metadata 
(e.g., bold title words or links in author names).

### Block-Level Elements (Body)

The `Block` type represents top-level content structures:

```haskell
-- | Top-level content blocks (paragraphs, headers, lists, etc.)
data Block
  = Plain [Inline]                             -- ^ Plain text, no paragraph tag
  | Para [Inline]                              -- ^ A paragraph
  | CodeBlock String                           -- ^ A block of code with attributes
  | RawBlock Format String                     -- ^ Raw content (HTML, LaTeX, etc.)
  | OrderedList ListAttributes [[Block]]       -- ^ Ordered list (1., 2., 3., ...)
  | BulletList [[Block]]                       -- ^ Unordered list (*, -, etc.)
  | DefinitionList [([Inline], [[Block]])]     -- ^ List of terms and definitions
  | Header Int [Inline]                        -- ^ A header (level, attrs, content) optional a list of block to create section
  | Section Int [Inline] [Block]               -- ^ A section with an inline header and a list of other section in the current section
  | Null                                       -- ^ No content (empty block)
  deriving (Show, Eq)
```
Each `Block` can contain nested `Inline` elements, and in some cases, even nested `Blocks` (e.g., in lists or quotes).

### Inline Elements
`Inline` elements define the smallest meaningful units of content, such as words, links, or formatting tokens:

```haskell
-- | Inline elements (words, emphasis, links, etc.)
data Inline
  = Str String                                 -- ^ Text string
  | Emph [Inline]                              -- ^ Emphasized text (italic)
  | Strong [Inline]                            -- ^ Strongly emphasized (bold)
  | Code String                                -- ^ Inline code
  | RawInline Format String                    -- ^ Raw inline content
  | Link [Inline] Target                       -- ^ Hyperlink (text + target)
  | Image [Inline] Target                      -- ^ Image (alt text + target)
  | Note [Block]                               -- ^ Footnote
  | Span [Inline]                              -- ^ Inline container with attributes
  deriving (Show, Eq)
```
This flexible design allows fine-grained styling and semantic annotation of textual content, 
supporting everything from simple strings to complex nested formatting.

### Targets for Links and Images
Links and images both use the `Target` type to define where they point:
```haskell
type Target = (String, String)  -- (URL, title/tooltip)
```
exmple: ```("https://example.com", "Click here")```

### List Attributes

Ordered lists can be customized using start numbers, styles, and delimiters.

```haskell
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)
```
- `Int`: Starting number of the list
- `ListNumberStyle`: Formatting of numbers (e.g., Roman numerals)
- `ListNumberDelim`: Delimiter style (e.g., period, parentheses)

### Format Identifiers

Format specifiers are used for raw blocks and inline elements to indicate the intended output format.

```haskell
newtype Format = Format String
  deriving (Show, Eq)
```
---
## Parsing

### Supported Parsers
The following input formats are supported:
- **Markdown**
- **JSON**
- **XML**

### Parsing Capabilities
The parsing process extracts and organizes document content into structured components. Key functionalities include:

#### Metadata Extraction
- **Title**: Captures the document's main title.
- **Author(s)**: Identifies the author(s) of the document.
- **Date**: Extracts the creation or publication date.

#### Block-Level Extraction
- **Headings**: Detects and processes section titles.
- **Paragraphs**: Identifies and structures text paragraphs.
- **Code Blocks**: Extracts and formats blocks of code.
- **Links**: Parses hyperlinks and their targets.
- **Images**: Extracts image references and metadata.
- **Lists**: Processes both ordered and unordered lists.

#### Inline Element Extraction
- **Bold Text**: Recognizes and formats strongly emphasized text.
- **Italic Text**: Identifies and formats emphasized text.
- **Inline Code**: Extracts and formats inline code snippets.
