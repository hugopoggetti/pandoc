{-
-- EPITECH PROJECT, 2022
-- markdownRender.hs
-- File description:
-- tt
-}

module Renders.DebugRender.DebugRender (debugRender) where
import Ast.Document

debugRender :: Document -> String 
debugRender (Document meta blocks) = 
    debugMeta meta ++ "\n" ++ debugBlocks blocks

debugBlock :: Block -> String
debugBlock it = show it ++ "\n"

debugBlocks :: [Block] -> String
debugBlocks = concatMap debugBlock

debugMeta :: Meta -> String
debugMeta = show
