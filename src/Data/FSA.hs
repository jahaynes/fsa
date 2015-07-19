{-# LANGUAGE Safe #-}

module Data.FSA where

import Data.FSA.Internal.Node
import Data.FSA.Internal.NodeState

import Data.List                    (partition)

-- | Return the language of this automaton.
language :: Node -> [String]
language = go []
    where
    go :: String -> Node -> [String]
    go buf (Node nodeState ls)
        | isTerminal nodeState = buf : next
        | otherwise = next
        where
        next = concatMap (\(k,v) -> go (buf++[k]) v) ls

-- | Create an automaton which accepts just the given string.
singleton :: String -> Node
singleton     [] = Node (terminal (nonConfluence newState)) []
singleton (x:xs) = Node (nonTerminal (nonConfluence newState)) [(x, singleton xs)] 

-- | Does this automaton accept the given string?
accepts :: Node -> String -> Bool
accepts (Node nodeState    _)     [] = isTerminal nodeState
accepts (Node         _ outs) (c:cs) =
    case dropWhile (\x -> fst x /= c) outs of
        []         -> False
        ((_,nn):_) -> accepts nn cs

-- | Insert a string into an automaton.
insert :: String -> Node -> Node  
insert = flip go
    where
    go (Node nodeState   []) [] = Node (terminal nodeState) []
    go (Node nodeState outs) [] = Node (confluence (terminal nodeState)) outs
    go n@(Node nodeState outs) (c:cs)
        | isConfluence nodeState = if accepts n (c:cs)
                                       then n
                                       else singleton (c:cs)
        | otherwise =
            let (matchedOut, otherOuts) = partition (\x -> fst x == c) outs
                matchedOut' =
                    case matchedOut of
                        []        -> (c, singleton cs)
                        ((_,m):_) -> (c, go m cs)
            in
            Node nodeState (matchedOut':otherOuts)
