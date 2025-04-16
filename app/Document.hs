{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-compressor-hugo.poggetti
-- File description:
-- Document
-}

module Document ( Document(Document)
                , Meta(Meta)
                , MetaValue(..)
                , Block(..)
                , Inline(..)
                , Attr
                , Target
                , QuoteType(..)
                , MathType(..)
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
  , metaOther    :: [(String, MetaValue)]      -- ^ Other custom metadata fields
  } deriving (Show, Eq)

-- | Values that can appear in metadata
data MetaValue
  = MetaString String                          -- ^ A simple string value
  | MetaBool Bool                              -- ^ A boolean value
  | MetaList [MetaValue]                       -- ^ A list of metadata values
  | MetaMap [(String, MetaValue)]              -- ^ A key-value map
  | MetaInlines [Inline]                       -- ^ Inline content (like formatted text)
  | MetaBlocks [Block]                         -- ^ Block content (like full paragraphs)
  deriving (Show, Eq)

-- | Top-level content blocks (paragraphs, headers, lists, etc.)
data Block
  = Plain [Inline]                             -- ^ Plain text, no paragraph tag
  | Para [Inline]                              -- ^ A paragraph
  | CodeBlock Attr String                      -- ^ A block of code with attributes
  | RawBlock Format String                     -- ^ Raw content (HTML, LaTeX, etc.)
  | BlockQuote [Block]                         -- ^ Block quote
  | OrderedList ListAttributes [[Block]]       -- ^ Ordered list (1., 2., 3., ...)
  | BulletList [[Block]]                       -- ^ Unordered list (*, -, etc.)
  | DefinitionList [([Inline], [[Block]])]     -- ^ List of terms and definitions
  | Header Int Attr [Inline]                   -- ^ A header (level, attrs, content)
  | HorizontalRule                             -- ^ Horizontal line (---)
  | Div Attr [Block]                           -- ^ A generic container with attributes
  | Null                                       -- ^ No content (empty block)
  deriving (Show, Eq)

-- | Inline elements (words, emphasis, links, etc.)
data Inline
  = Str String                                 -- ^ Text string
  | Emph [Inline]                              -- ^ Emphasized text (italic)
  | Strong [Inline]                            -- ^ Strongly emphasized (bold)
  | Strikeout [Inline]                         -- ^ Strikethrough
  | Superscript [Inline]                       -- ^ Superscript text
  | Subscript [Inline]                         -- ^ Subscript text
  | SmallCaps [Inline]                         -- ^ Small caps text
  | Quoted QuoteType [Inline]                  -- ^ Quoted text
  | Cite [Inline]                              -- ^ Citation with bibliography reference
  | Code Attr String                           -- ^ Inline code
  | Space                                      -- ^ Space between words
  | SoftBreak                                  -- ^ Soft line break (no new paragraph)
  | LineBreak                                  -- ^ Hard line break (new line)
  | Math MathType String                       -- ^ Math content
  | RawInline Format String                    -- ^ Raw inline content
  | Link Attr [Inline] Target                  -- ^ Hyperlink (text + target)
  | Image Attr [Inline] Target                 -- ^ Image (alt text + target)
  | Note [Block]                               -- ^ Footnote
  | Span Attr [Inline]                         -- ^ Inline container with attributes
  deriving (Show, Eq)

-- | Common attributes used in code, divs, spans, etc.
-- (identifier, list of classes, key-value attributes)
type Attr = (String, [String], [(String, String)])

-- | Target for links or images: (URL, title/tooltip)
type Target = (String, String)

-- | Quotation style
data QuoteType = SingleQuote | DoubleQuote
  deriving (Show, Eq)

-- | Whether math is inline or displayed
data MathType = DisplayMath | InlineMath
  deriving (Show, Eq)

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
