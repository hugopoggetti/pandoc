module Main (main) where

-- import Lib
import Renders.MarkdownRender.MarkdownRender (markdownRender)
import Renders.JsonRender.JsonRender (jsonRender)
import Renders.XmlRender.XmlRender (xmlRender)
import Renders.DebugRender.DebugRender (debugRender)
import Ast.Document

-- exampleDoc :: Document
-- exampleDoc = Document meta blocks
--   where
--     meta = Meta
--       { metaTitle = [Str "Syntaxe MARKDOWN"]
--       , metaAuthors = [[Str "Fornes Leo"]]
--       , metaDate = [Str "2024-01-01"]
--       }
--
--     blocks =
--       [ Plain [Str "This document is a simple example of the MARKDOWN syntax."]
--       , Plain [Str "Every syntax element is displayed in this document."]
--       , Header 1 [Str "header 1"]
--       , Para [Str "This is a basic paragraph with text."]
--       , Para
--           [ Str "This is a paragraph with" 
--           , Strong [Str "bold"], Str ", "
--           , Emph [Str "italic"], Str "and "
--           , Code "code", Str " text."
--           ]
--       , Header 2 [Str "header 2"]
--       , CodeBlock "This is a code block."
--       , BulletList
--           [ [Para [Str "list item 1"]]
--           , [Para [Str "list item 2"]]
--           , [Para [Str "list item 3"]]
--           ]
--       , Para
--           [ Str "This is a paragraph with"
--           , Link [Str "link"] ("https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley", "")
--           , Str "."
--           ]
--       , Para
--           [ Str "This is a paragraph with an image"
--           , Image [Str "Text to replace image"] 
--             ("https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png", "")
--           , Str "."
--           ]
--       , Header 4 [Str "header 4"]
--       , Para
--           [ Str "Every syntax element can be use separately or combined."
--           ]
--       , Para
--           [ Str "Think about the different possible combinations." ]
--       , Para
--           [ Str "All combined syntax elements aren't displayed in this document." ]
--       ]

exampleDoc :: Document
exampleDoc = Document meta blocks
  where
    meta = Meta
      { metaTitle = [Str "Syntaxe MARKDOWN"]
      , metaAuthors = [[Str "Fornes Leo"]]
      , metaDate = [Str "2024-01-01"]
      }

    blocks =
      [ Header 1 [Str "Introduction"]
          , Para [Str "This document explains basic Markdown syntax."]
          , Section [Str "Syntax"]
              [ Header 2 [Str "Paragraphs"]
                  , Para [Str "This is a basic paragraph in Markdown."]
              , Header 2 [Str "Lists"]
                  , BulletList
                      [ [Para [Str "Item 1"]]
                      , [Para [Str "Item 2"]]
                      ]
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
    putStrLn $ replicate 50 '-'
    putStrLn "XML"
    putStrLn $ replicate 50 '-'
    putStrLn $ xmlRender exampleDoc
    putStrLn $ replicate 50 '-'
    putStrLn "DEBUG"
    putStrLn $ replicate 50 '-'
    putStrLn $ debugRender exampleDoc


