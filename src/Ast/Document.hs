{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-compressor-hugo.poggetti
-- File description:
-- Document
-}

module Ast.Document ( Document(Document)
                , Meta(..)
                , Block(..)
                , Inline(..)
                , Target
                , Format(..)
                , ListAttributes
                , ListNumberDelim(..)
                , ListNumberStyle(..)
                ) where

-- | Represents a complete document with metadata and content blocks.
data Document = Document Meta [Block]
  deriving (Show, Eq)

-- | Metadata of the document: title, authors, date, etc.
data Meta = Meta
  { metaTitle    :: [Inline]                   -- ^ Document title
  , metaAuthors  :: [[Inline]]                 -- ^ List of authors (each a list of inline elements)
  , metaDate     :: [Inline]                   -- ^ Date of the document
  } deriving (Show, Eq)

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
  | Section [Inline] [Block]                   -- ^ A section with an inline header and a list of other section
  | Null                                       -- ^ No content (empty block)
  deriving (Show, Eq)

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

-- | Target for links or images: (URL, title/tooltip)
type Target = (String, String)

-- | Format specifier (e.g. "html", "latex")
newtype Format = Format String
  deriving (Show, Eq)

-- | Ordered list attributes: (start number, style, delimiter)
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)

-- | Number styles for ordered lists
data ListNumberStyle
  = DefaultStyle | Example | Decimal | LowerRoman | UpperRoman
  | LowerAlpha | UpperAlpha
  deriving (Show, Eq)

-- | Delimiters for list numbers
data ListNumberDelim
  = DefaultDelim | Period | OneParen | TwoParens
  deriving (Show, Eq)
