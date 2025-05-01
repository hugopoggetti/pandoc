{-
-- EPITECH PROJECT, 2022
-- jsonRender.hs
-- File description:
-- tt
-}

module Renders.HtmlRender.HtmlRender (htmlRender) where
import Ast.Document
import Data.List (intercalate)

-- | html document render
htmlRender :: Document -> String
htmlRender (Document meta block) = 
    "<!DOCTYPE html>\n<html lang=\"fr\">\n" ++  htmlRenderTitle meta 
    ++ "<body>\n<main>\n" ++ htmlRenderBlocks block 
        ++ "</body>\n</main>\n</html>\n"

-- | html Inlines render
htmlRenderInlines :: [Inline] -> String
htmlRenderInlines = concatMap htmlRenderInline

-- | Recursive function to render inline in html format
htmlRenderInline :: Inline -> String
htmlRenderInline (Str s) = s
htmlRenderInline (Emph xs) = "<em>" ++ htmlRenderInlines xs ++ "</em>"
htmlRenderInline (Strong xs) = "<stong>" ++ htmlRenderInlines xs ++ "</strong>"
htmlRenderInline (Code s) = "<code>" ++ s ++ "</code>" 
htmlRenderInline (Link i (ur, title)) = "<a href=" ++ show ur ++ ">"
    ++ htmlRenderInlines i ++ title ++ "</a>"
htmlRenderInline (Image i (ur, title)) = "<img src=" ++ show ur ++ "atl="
    ++ show (htmlRenderInlines i ++ title) ++ ">"
htmlRenderInline _ = ""

-- | Rende list of block
htmlRenderBlocks :: [Block] -> String
htmlRenderBlocks =  concatMap htmlRenderBlock

-- | Rende list of block for list 
htmlRenderBlocksl :: [Block] -> String
htmlRenderBlocksl b = "<li>" ++ concatMap htmlRenderBlock b ++ "</li>\n"

-- | Recursive function to render a block
htmlRenderBlock :: Block -> String
htmlRenderBlock (Plain text) = htmlRenderInlines text
htmlRenderBlock (Para text) =
    "<p>" ++ htmlRenderInlines text ++ "</p>"
htmlRenderBlock (CodeBlock content) = "<pre>\n<code>" 
    ++ content ++ "</code>\n</pre>"
htmlRenderBlock (BulletList blocks) = "<ul>" ++  
    concatMap htmlRenderBlocksl blocks ++ "</ul>\n"
htmlRenderBlock (Section lev xs ys) = 
    "<section>\n" ++ "<h" ++ show lev ++ ">" 
    ++ htmlRenderInlines xs ++ "</h"++ show lev ++">\n"
    ++ htmlRenderBlocks ys ++ "</section>\n" 
htmlRenderBlock _ = ""

-- | Render document title
htmlRenderTitle :: Meta -> String
htmlRenderTitle (Meta title auth date) =
    let 
        titleStr = "<title>" ++ show (htmlRenderInlines title) ++ "</title>\n"
        authStr = if null auth then "" else 
            "<author>" ++ intercalate ", " (map htmlRenderInlines auth) 
                ++ "</author>\n"
        dateStr = if null date then "" else 
            "<date>" ++ htmlRenderInlines date ++ "</date>\n"
    in "<head>\n" ++ titleStr ++ authStr ++ dateStr ++ "</head>\n"
