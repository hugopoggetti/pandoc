module Main (main) where

-- import Lib
import Renders.MarkdownRender.MarkdownRender (markdownRender)
import Renders.JsonRender.JsonRender (jsonRender)
import Ast.Document

exampleDoc :: Document
exampleDoc = Document meta blocks
  where
    meta = Meta
      { metaTitle = [Str "Advanced Document"]
      , metaAuthors = [[Str "Ada Lovelace"], [Str "Alan Turing"]]
      , metaDate = [Str "2025-04-20"]
      }
    citation1 = Citation
      { citationId = "doe2024"
      , citationPrefix = [Str "see"]
      , citationSuffix = [Str "for details"]
      , citationMode = NormalCitation
      , citationNoteNum = 1
      , citationHash = 123
      }

    blocks =
      [ Header 2 ("intro", [], []) [Str "Introduction"]
      , Para
          [ Str "Welcome to the document,", Space
          , Emph [Str "written"], Space
          , Str "in", Space, Strong [Str "Markdown"], Str "."
          ]
      , BlockQuote
          [ Para [Str "This is a quote with", Space, Code ("", [], []) "inline code", Str "."]
          ]
      , OrderedList (1, Decimal, Period)
          [ [Para [Str "First step"]]
          , [Para [Str "Second step"]]
          ]
      , BulletList
          [ [Plain [Str "Bullet one"]]
          , [Plain [Str "Bullet two with", Space, Link ("", [], []) [Str "a link"] ("https://example.com", "Example")]]
          ]
      , DefinitionList
          [ ([Str "Haskell"], [[Para [Str "A purely functional language."]]])
          , ([Str "Pandoc"], [[Para [Str "Universal document converter."]]])
          ]
      , Para
          [ Quoted DoubleQuote [Str "To be, or not to be"]
          , Str ", that is the question."
          ]
      , Para
          [ Cite [citation1] [Str "A cited statement"]
          ]
      , Para
          [ Math InlineMath "x^2 + y^2 = z^2", Str " is Pythagorean."
          ]
      , Para
          [ RawInline (Format "html") "<span style='color:red;'>Raw HTML</span>" ]
      , CodeBlock ("", ["haskell"], []) "main = putStrLn \"Hello!\""
      , RawBlock (Format "latex") "\\LaTeX{} only content"
      , HorizontalRule
      , Div ("custom-div", ["highlight"], [("data-extra", "42")])
          [ Para [Str "Inside a div block."]
          , CodeBlock ("code-block", ["json"], []) "{ \"hello\": \"world\" }"
          ]
      , Para
          [ Image ("", ["img-class"], [("width", "100%")])
              [Str "A diagram"]
              ("diagram.png", "Diagram image")
          ]
      , Para
          [ Note
              [ Para [Str "This is a footnote with", Space, Strong [Str "bold"], Str " text."] ]
          ]
      , Para
          [ Subscript [Str "sub"], Space
          , Superscript [Str "super"], Space
          , Strikeout [Str "removed"], Space
          , SmallCaps [Str "caps"], Space
          , Span ("", ["highlight"], []) [Str "highlighted span"]
          ]
      ]

main :: IO ()
main = do
    putStrLn $ replicate 50 '-'
    putStrLn "Markdown"
    putStrLn $ replicate 50 '-'
    putStrLn $ markdownRender exampleDoc
    putStrLn $ replicate 50 '-'
    putStrLn "JSON"
    putStrLn $ replicate 50 '-'
    putStrLn $ jsonRender exampleDoc

-- main :: IO ()
-- main = do
--   let meta = Meta
--         [Str "My Document"]
--         -- [[Str "Alice"], [Str "Bob"]]
--         -- [Str "2025-04-16"]
--         []
--         []
--         []
--       blocks = 
--         [ Plain [Str "This is a simple example"] ]
--       -- blocks =
--       --   [ Header 1 ("", [], []) [Str "Introduction"]
--       --   , Para [Str "This is a simple paragraph with", Space, Emph [Str "emphasis"], Str "."]
--       --   , BulletList
--       --       [ [Para [Str "First item"]]
--       --       , [Para [Str "Second item with", Space, Strong [Str "strong text"]]]
--       --       ]
--       --   , OrderedList (1, Decimal, Period)
--       --       [ [Para [Str "Step one"]]
--       --       , [Para [Str "Step two"]]
--       --       ]
--       --   , CodeBlock ("", ["haskell"], []) "main = putStrLn \"Hello, world!\""
--       --   ]
--       doc = Document meta blocks
--   putStrLn $ replicate 50 '-'
--   putStrLn "Markdown" 
--   putStrLn $ replicate 50 '-'
--   putStrLn $ markdownRender doc
--   putStrLn $ replicate 50 '-'
--   putStrLn "Json"
--   putStrLn $ replicate 50 '-'
--   putStrLn $ jsonRender doc

