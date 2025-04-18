module Main (main) where

import Lib
import Renders.MarkdownRender.MarkdownRender (markdownRender)
import Definitions.Document

main :: IO ()
main = do
  let meta = Meta
        [Str "My Document"]
        [[Str "Alice"], [Str "Bob"]]
        [Str "2025-04-16"]
        []
      blocks =
        [ Header 1 ("", [], []) [Str "Introduction"]
        , Para [Str "This is a simple paragraph with", Space, Emph [Str "emphasis"], Str "."]
        , BulletList
            [ [Para [Str "First item"]]
            , [Para [Str "Second item with", Space, Strong [Str "strong text"]]]
            ]
        , OrderedList (1, Decimal, Period)
            [ [Para [Str "Step one"]]
            , [Para [Str "Step two"]]
            ]
        , CodeBlock ("", ["haskell"], []) "main = putStrLn \"Hello, world!\""
        ]
      doc = Document meta blocks
  putStrLn $ markdownRender doc
