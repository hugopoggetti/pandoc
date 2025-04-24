module Main (main) where

-- import Lib
import Renders.MarkdownRender.MarkdownRender (markdownRender)
import Renders.JsonRender.JsonRender (jsonRender)
import Renders.XmlRender.XmlRender (xmlRender)
import Renders.DebugRender.DebugRender (debugRender)
import Ast.Document

exampleDoc :: Document
exampleDoc = Document
  (Meta
    [Str "Syntaxe JSON"]
    [[Str "Fornes Leo"]]
    [Str "2024-01-01"]
  )
  [ Para [Str "This document is a simple example of the JSON syntax."]
  , Para [Str "Every syntax element is displayed in this document."]
  , Section 1
      [Str "header 1"] 
      [ Para [Str "This is a basic paragraph with text."]
      , Para
          [ Str "This is a paragraph with "
          , Strong [Str "bold"]
          , Str ", "
          , Emph [Str "italic"]
          , Str " and "
          , Code "code"
          , Str " text."
          ]
      , Section 2
          [Str "header 2"]
          [ CodeBlock "This is a code block."
          , BulletList
              [ [Para [Str "list item 1"]]
              , [Para [Str "list item 2"]]
              , [Para [Str "list item 3"]]
              ]
          , Para
              [ Str "This is a paragraph with a "
              , Link [Str "link"]
                  ("https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley", "")
              , Str "."
              ]
          , Para
              [ Str "This is a paragraph with an image "
              , Image [Str "Text to replace image"]
                  ("https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png", "")
              , Str "."
              ]
          , Section 3
              []
              [ Section 4
                  [Str "header 4"]
                  [ Para [Str "Every syntax element can be use separately or combined."]
                  , Para [Str "Think about the different possible combinations."]
                  , Para [Str "All combined syntax elements aren't displayed in this document."]
                  ]
              ]
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


