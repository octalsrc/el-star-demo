{-# LANGUAGE FlexibleContexts #-}

module Automata.DFA where

import Automata.DFA.Alphabet
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

type DFA a = (Node, Gr Bool (AElem a))

mkDFA :: a -> [LNode Bool] -> Node -> [LEdge (AElem a)] -> DFA a
mkDFA _ ss i es = (i, mkGraph ss es)

mkDFA' :: AClass a => a -> [LNode Bool] -> Node -> [LEdge Char] -> Maybe (DFA a)
mkDFA' a ss i es = fmap (\es' -> mkDFA a ss i es') (mapM (raiseEdge a) es)

raiseEdge :: AClass a => a -> LEdge Char -> Maybe (LEdge (AElem a))
raiseEdge a (s,s',c) = fmap (\e -> (s,s',e)) (charToElem a c)

example :: Maybe (DFA Alphabet')
example = let states = [(1,True)
                       ,(2,False)]
              edges = [(1,1,'a')
                      ,(1,2,'b')
                      ,(2,2,'a')
                      ,(2,2,'b')]
              alph = mkAlphabet "ab"
          in mkDFA' alph states 1 edges
