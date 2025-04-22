{-
-- EPITECH PROJECT, 2022
-- markdownRender.hs
-- File description:
-- tt
-}

module Renders.MarkdownRender.MarkdownRender (markdownRender) where
import Ast.Document
import Data.List (intercalate)

-- | rendr Attr exemple: ### Title {#id .class1 .class2 key="value"}
markRenderAttr :: Attr -> String
markRenderAttr ("", [], []) = ""
markRenderAttr (atid, classes, kv) =
  " {" ++ unwords (idStr ++ classStr ++ kvStr) ++ "}"
  where
    idStr = if null atid then [] else ["#" ++ atid]
    classStr = map ("." ++) classes
    kvStr = map (\(k,v) -> k ++ "=" ++ show v) kv

-- | rendr citation exemple: [@autor2020]
markRenderCitation :: Citation -> String
markRenderCitation citation =
  case citationMode citation of
    SuppressAuthor -> "-@" ++ citationId citation
    AuthorInText   -> citationId citation
    NormalCitation -> "@" ++ citationId citation

-- | get Language of the first classes for a codeBlock
getLanguageAttr :: Attr -> String
getLanguageAttr (_,[],_) = ""
getLanguageAttr (_,x:_,_) = x

-- | get link or img string
getMarkdownLink :: Attr -> [Inline] -> Target -> String
getMarkdownLink attr inlines (url, title) = 
    "[" ++ markRenderInlines inlines ++ "](" ++ url 
    ++ show title ++ ")" ++ markRenderAttr attr

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
markRenderInline (Strikeout xs) = "~~" ++ markRenderInlines xs ++ "~~"
markRenderInline (Superscript xs) =  "^" ++ markRenderInlines xs ++ "^"
markRenderInline (Subscript xs) = "~" ++ markRenderInlines xs ++ "~"
-- markRenderInline (SmallCaps xs) = TODO: compare with pandoc 
markRenderInline (Quoted SingleQuote xs) =  "'" ++ markRenderInlines xs ++ "'"
markRenderInline (Quoted DoubleQuote xs) = "\"" ++ markRenderInlines xs ++ "\""
markRenderInline (Cite citations inlines) = markRenderInlines inlines ++
    " [" ++ intercalate ", " (map markRenderCitation citations) ++ "]"
markRenderInline (Code attr s) = "`" ++ s ++ "`" ++ markRenderAttr attr
markRenderInline Space = " "
markRenderInline SoftBreak = "\n"
markRenderInline LineBreak = "\n"
markRenderInline (Math InlineMath s) = "\\(" ++ s ++ "\\)"
markRenderInline (Math DisplayMath s) = "$$" ++ s ++ "$$"
markRenderInline (RawInline fmt s) = markRenderRawInline fmt s 
markRenderInline (Link a i t) = getMarkdownLink a i t
markRenderInline (Image a i t) = "!" ++ getMarkdownLink a i t
markRenderInline (Note blocks) = markRenderBlocks blocks
markRenderInline (Span a i) = markRenderInlines i ++ markRenderAttr a
markRenderInline _ = ""

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
markRenderBlock (CodeBlock attr content) = "```" ++ getLanguageAttr attr
    ++ "\n" ++ content ++ "\n```" ++ markRenderAttr attr
markRenderBlock (RawBlock fmt str) = markRenderRawInline fmt str
markRenderBlock (BlockQuote bs) = ">" ++ unlines (map markRenderBlock bs)
markRenderBlock (OrderedList lAttr blocks) = markRenderList lAttr blocks
markRenderBlock (BulletList blocks) = markRenderBulletList blocks
markRenderBlock (DefinitionList items) = markRenderDefList items
markRenderBlock (Header lev attr content) = replicate lev '#' ++ 
    " " ++ markRenderInlines content ++ markRenderAttr attr ++"\n"
markRenderBlock HorizontalRule = "---\n"
markRenderBlock (Div attr blocks) =
  "<div" ++ markRenderAttr attr ++ ">\n"
  ++ concatMap markRenderBlock blocks
  ++ "</div>\n"
markRenderBlock _ = ""

-- | header generation with Meta 
markdownRenderTitle :: Meta -> String
markdownRenderTitle (Meta title auth date) = 
    let 
        titleStr  = if null title then "" else "title: " 
            ++ markRenderInlines title ++ "\n"
        authStr   = if null auth then "" else "Authors: " ++
            intercalate ", " (map markRenderInlines auth) ++ "\n"
        dateStr   = if null date then "" else "Date: "
            ++ markRenderInlines date ++ "\n"
    in "---\n" ++ titleStr ++ authStr ++ dateStr ++ "---\n\n"

-- | Render entire Document into Markdown format
markdownRender :: Document -> String
markdownRender (Document meta blocks) = 
    markdownRenderTitle meta ++ markdownRenderBody blocks 

-- | Render body of Document
markdownRenderBody :: [Block] -> String
markdownRenderBody  = markRenderBlocks 
