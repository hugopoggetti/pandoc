{-
-- EPITECH PROJECT, 2022
-- markdownRender.hs
-- File description:
-- tt
-}

module MarkdownRender ( markdownRender 
                      ) where 

import Document

markdownRender :: Document -> String
markdownRender (Document meta blocks) = markdownRenderHeader meta

markdownRenderHeader :: Meta -> String
markdownRenderHeader _ = "header"
