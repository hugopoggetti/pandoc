{-
-- EPITECH PROJECT, 2022
-- markdownRender.hs
-- File description:
-- tt
-}

module Renders.MarkdownRender.MarkdownRender (markdownRender) where
import Definitions.Document
import Data.List (intercalate)

-- | Render entire pars Document into Markdown format
markdownRender :: Document -> String
markdownRender (Document meta blocks) = markdownRenderHeader meta ++ markdownRenderBody blocks 

-- | Render of Markdown Inline list
markRenderInlines :: [Inline] -> String
markRenderInlines = concatMap markRenderInline

-- TODO: add code, span, Note... -> temp verions 
-- | Recursive func to generate inline in markdown format with all conventions 
markRenderInline :: Inline -> String
markRenderInline (Str s) = s
markRenderInline Space = " "
markRenderInline SoftBreak = "\n"
markRenderInline LineBreak = "\n"
markRenderInline (Emph xs) = "*" ++ markRenderInlines xs ++ "*"
markRenderInline (Strong xs) = "**" ++ markRenderInlines xs ++ "**"
markRenderInline (Code _ s) = "`" ++ s ++ "`"
markRenderInline (Link _ xs (url, _)) = "[" ++ markRenderInlines xs ++ "](" ++ url ++ ")"
markRenderInline (Image _ xs (url, _)) = "![" ++ markRenderInlines xs ++ "](" ++ url ++ ")"
markRenderInline _ = ""

-- | Render list of block into Markdown format
markRenderBlocks :: [Block] -> String
markRenderBlocks = concatMap markRenderBlock

-- TODO: add other method 
-- | Render Block 
markRenderBlock :: Block -> String
markRenderBlock (Plain text) = markRenderInlines text
markRenderBlock (Para text) = markRenderInlines text
markRenderBlock _ = ""

-- TODO: add key-values - pairs for other meta values 
-- | header generation with Meta 
markdownRenderHeader :: Meta -> String
markdownRenderHeader (Meta title auth date _) = 
    let 
        titleStr  = if null title then "" else "---\n" ++ markRenderInlines title ++ "\n---\n\n"
        authStr   = if null auth then "" else "**Authors:** " ++
            intercalate ", " (map markRenderInlines auth) ++ "\n\n"
        dateStr   = if null date then "" else "**Date:** " ++ markRenderInlines date ++ "\n\n"
    in titleStr ++ authStr ++ dateStr

markdownRenderBody :: [Block] -> String
markdownRenderBody  = markRenderBlocks 

