{-# LANGUAGE Safe #-}

module Data.FSA.Internal.Node (
    Node (Node)
    ) where

import Data.FSA.Internal.NodeState (NodeState, isTerminal, isConfluence)

data Node = Node NodeState [(Char, Node)]

instance Show Node where
    show = go 0
        where
        go depth (Node nodeState ls) =
            let n = if isTerminal nodeState
                        then "T"
                        else "N"
                n' = if isConfluence nodeState
                         then '{':n++"}"
                         else '(':n++")"
                children = map snd ls
            in
            replicate depth ' ' ++ n' ++ " " ++ map fst ls ++ "\n" ++ concatMap (go (depth+1)) children
