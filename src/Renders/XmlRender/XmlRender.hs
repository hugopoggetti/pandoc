{-
-- EPITECH PROJECT, 2022
-- jsonRender.hs
-- File description:
-- tt
-}

module Renders.XmlRender.XmlRender (xmlRender) where
import Ast.Document
import Data.List (intercalate)

-- | xml document render
xmlRender :: Document -> String
xmlRender (Document meta block) = "<document>\n" ++  xmlRenderTitle meta 
    ++ "<body>" ++ xmlRenderBlocks block ++ "</body>\n</document>\n"

-- | xml Inlines render
xmlRenderInlines :: [Inline] -> String
xmlRenderInlines = concatMap xmlRenderInline

-- | Recursive function to render inline in xml format
xmlRenderInline :: Inline -> String
xmlRenderInline (Str s) = s
xmlRenderInline (Emph xs) = "<italic>" ++ xmlRenderInlines xs ++ "</italic>\n"
xmlRenderInline (Strong xs) = "<bold>" ++ xmlRenderInlines xs ++ "</bold>\n"
xmlRenderInline (Code s) = "<code>" ++ s ++ "</code>" 
xmlRenderInline (Link i (ur, title)) = "<link url=" ++ show ur ++ ">"
    ++ xmlRenderInlines i ++ title ++ "</link>\n"
xmlRenderInline (Image i (ur, title)) = "<image url=" ++ show ur ++ ">"
    ++ xmlRenderInlines i   ++ title ++ "</image>\n"
xmlRenderInline _ = ""

-- | Rende list of block
xmlRenderBlocks :: [Block] -> String
xmlRenderBlocks =  concatMap xmlRenderBlock

-- | Recursive function to render a block
xmlRenderBlock :: Block -> String
xmlRenderBlock (Plain text) = xmlRenderInlines text
xmlRenderBlock (Para text) = 
    "<paragraph>" ++ xmlRenderInlines text ++ "</paragraph>\n"
xmlRenderBlock (CodeBlock content) = "<codeblock>\n<paragraph>" 
    ++ content ++ "</paragraph>\n</codeblock>\n"
xmlRenderBlock (BulletList blocks) = "<list>" ++  
    concatMap xmlRenderBlocks blocks ++ "</list>\n"
xmlRenderBlock (Header _ content) = "title=" ++ show (xmlRenderInlines content)
xmlRenderBlock (Section _ xs ys) = 
    "<section title=" ++ show (xmlRenderInlines xs) ++ ">\n"
    ++ xmlRenderBlocks ys ++ "</section>\n" 
xmlRenderBlock _ = ""

-- | Render document title
xmlRenderTitle :: Meta -> String
xmlRenderTitle (Meta title auth date) =
    let 
        titleStr = "<header title=" ++ show (xmlRenderInlines title) ++ ">\n"
        authStr = if null auth then "" else 
            "<author>" ++ intercalate ", " (map xmlRenderInlines auth) 
                ++ "</author>\n"
        dateStr = if null date then "" else 
            "<date>" ++ xmlRenderInlines date ++ "</date>\n"
    in titleStr ++ authStr ++ dateStr ++ "</header>\n"
