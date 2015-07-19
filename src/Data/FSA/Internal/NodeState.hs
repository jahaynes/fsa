{-# LANGUAGE Safe #-}

module Data.FSA.Internal.NodeState (
    NodeState,
    newState,
    terminal,
    nonTerminal,
    isTerminal,
    confluence,
    nonConfluence,
    isConfluence
    ) where

import Data.Word (Word8)
import Data.Bits (setBit, testBit, clearBit)

type NodeState = Word8

terminalBit :: Int
terminalBit = 0

confluenceBit :: Int
confluenceBit = 1

-- | Create a default NodeState (no flags set).
newState :: NodeState
newState = 0

-- | Mark a NodeState as terminal.
terminal :: NodeState -> NodeState
terminal nodeState = setBit nodeState terminalBit

-- | Mark a NodeState as non-terminal.
nonTerminal :: NodeState -> NodeState
nonTerminal nodeState = clearBit nodeState terminalBit

-- | Check if a NodeState is terminal.
isTerminal :: NodeState -> Bool
isTerminal nodeState = testBit nodeState terminalBit

-- | Mark a NodeState as confluence.
confluence :: NodeState -> NodeState
confluence nodeState = setBit nodeState confluenceBit

-- | Mark a NodeState as non-confluence.
nonConfluence :: NodeState -> NodeState
nonConfluence nodeState = clearBit nodeState confluenceBit

-- | Check if a NodeState is a confluence NodeState.
isConfluence :: NodeState -> Bool
isConfluence nodeState = testBit nodeState confluenceBit
