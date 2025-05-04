{-
-- EPITECH PROJECT, 2022
-- markdownRender.hs
-- File description:
-- tt
-}

module Renders.MarkdownRender.MarkdownRender (markdownRender) where
import Ast.Document
import Data.List (intercalate)

-- | get link or img string
getMarkdownLink ::[Inline] -> Target -> String
getMarkdownLink inlines (url, _) = 
    "[" ++ markRenderInlines inlines ++ "](" ++ url 
    ++ ")"

markRenderRawInline :: Format -> String -> String
markRenderRawInline (Format fmt) s = 
    case fmt of
        "json"  -> "{\n" ++ s ++ "\n}"
        "xml"   -> "<raw>" ++ s ++ "</raw>"
        "yaml"  -> "---\n" ++ s ++ "\n---\n"
        _       -> s ++ "\n"

-- | Render of Markdown Inline list
markRenderInlines :: [Inline] -> String
markRenderInlines = concatMap markRenderInline

-- | Recursive func to generate inline in markdown format with all conventions 
markRenderInline :: Inline -> String
markRenderInline (Str s) = s
markRenderInline (Emph xs) = "*" ++ markRenderInlines xs ++ "*"
markRenderInline (Strong xs) = "**" ++ markRenderInlines xs ++ "**"
markRenderInline (Code s) = "`" ++ s ++ "`"
markRenderInline (RawInline fmt s) = markRenderRawInline fmt s 
markRenderInline (Link i t) = getMarkdownLink i t
markRenderInline (Image i t) = "!" ++ getMarkdownLink i t
markRenderInline (Note blocks) = markRenderBlocks blocks
markRenderInline (Span i) = markRenderInlines i

-- | Render list of block into Markdown format
markRenderBlocks :: [Block] -> String
markRenderBlocks = concatMap markRenderBlock

-- | Render OrderedList 
markRenderList :: ListAttributes -> [[Block]] -> String
markRenderList (start,_,_) blocks = 
    unlines $ zipWith renderItem [start..] blocks
  where
    renderItem n blockss =
      show n ++ ". " ++ indent (markRenderBlocks blockss)
    indent = unlines . map ("   " ++) . lines

-- | Render BulletList
markRenderBulletList :: [[Block]] -> String
markRenderBulletList blocks = unlines $ map renderItem blocks
      where
        renderItem block =
          "- " ++ indent (markRenderBlocks block)
        indent = unlines . map ("  " ++) . lines

-- | Render Definitions-List
markRenderDefList :: [([Inline], [[Block]])] -> String
markRenderDefList items = unlines $ map renderEntry items
  where
    renderEntry (term, defs) =
      markRenderInlines term ++ "\n"
      ++ concatMap renderDef defs
    renderDef def =
      ":   " ++ indent (markRenderBlocks def)
    indent = unlines . map ("    " ++) . lines

-- | Render Block 
markRenderBlock :: Block -> String
markRenderBlock (Plain text) = markRenderInlines text ++ "\n"
markRenderBlock (Para text) = markRenderInlines text ++ "\n"
markRenderBlock (CodeBlock content) = "```"
    ++ "\n" ++ content ++ "\n```\n"
markRenderBlock (RawBlock fmt str) = markRenderRawInline fmt str
markRenderBlock (OrderedList lAttr blocks) = markRenderList lAttr blocks
markRenderBlock (BulletList blocks) = markRenderBulletList blocks
markRenderBlock (DefinitionList items) = markRenderDefList items
markRenderBlock (Header lev content) = replicate lev '#' ++ 
    " " ++ markRenderInlines content ++"\n"
markRenderBlock (Section _ [] ys) = markdownRenderBody ys
markRenderBlock (Section _ [Str ""] ys) = markdownRenderBody ys
markRenderBlock (Section level xs ys) = replicate level '#' ++ " " 
    ++ markRenderInlines xs ++ "\n" ++ markdownRenderBody ys
markRenderBlock _ = ""

-- | header generation with Meta 
markdownRenderTitle :: Meta -> String
markdownRenderTitle (Meta title auth date) = 
    let 
        titleStr  = if null title then "" else "title: " 
            ++ markRenderInlines title ++ "\n"
        authStr   = if null auth then "" else "author: " ++
            intercalate ", " (map markRenderInlines auth) ++ "\n"
        dateStr   = if null date then "" else "date: "
            ++ markRenderInlines date ++ "\n"
    in "---\n" ++ titleStr ++ authStr ++ dateStr ++ "---\n\n"

-- | Render entire Document into Markdown format
markdownRender :: Document -> String
markdownRender (Document meta blocks) = 
    markdownRenderTitle meta ++ markdownRenderBody blocks 

-- | Render body of Document
markdownRenderBody :: [Block] -> String
markdownRenderBody  = markRenderBlocks 
