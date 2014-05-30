module Huffman (huffman) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Map (empty, insert, insertWith, Map, toList)
import Data.Maybe
import Data.PQueue.Min as PQ

-- Only exported function
-- Takes a list of symbols
-- and returns a map of each
-- symbol to its Huffman encoded
-- bit-string
huffman symbols = encodingMap
  where
    histogram = counts Data.Map.empty symbols
    pq = pqFromMap histogram
    huffmanTree = treeFromPQ pq
    encodingMap = traverseTree huffmanTree

-- Tree can be Empty or
-- a Node with data, left, and right
-- In this case, data is a 2-tuple of (weight, symbol)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)
nodeData (Node a _ _) = a

-- Takes a Map and a list and returns
-- a Map of each element in the list
-- to its frequency of occurrence
counts :: (Ord k) => Map k Int -> [k] -> Map k Int
counts m []     = m
counts m (x:xs) = counts newMap xs
  where
    newMap = insertWith (\new -> \old -> new+old) x 1 m

-- Sorts a Map in ascending order based on frequency
-- The result is used to simulate the heap for building
-- the Huffamn encoding tree
--heapifyMap :: Map a Int -> [(a, Int)]
--heapifyMap m = sortBy (compare `on` snd) (toList m)
pqFromMap m = pqFromMap' mList PQ.empty
  where
    mList = sortBy (compare `on` snd) (Data.Map.toList m)

pqFromMap' []     pq = pq
pqFromMap' (x:xs) pq = pqFromMap' xs newpq
  where
    newpq = PQ.insert newNode pq
    newNode = Node (snd x, Just (fst x)) Empty Empty

-- Takes a priority queue and turns it into
-- a Huffman encoding tree
treeFromPQ pq = treeFromPQ' pq Empty

treeFromPQ' pq root = 
  case PQ.size pq > 2 of
    -- Still have nodes to process
    True  -> treeFromPQ' (PQ.insert newRoot (PQ.drop 2 pq)) newRoot
    -- At the very end
    False -> newRoot
  where
    -- Place the first popped node on the left
    nodes = PQ.take 2 pq
    leftNode = head nodes
    leftData = nodeData leftNode
    leftFreq = fst leftData
    rightNode = head . tail $ nodes
    rightData = nodeData rightNode
    rightFreq = fst rightData
    newData = (leftFreq + rightFreq, Nothing)
    -- Arbitrary rule to always place the actual symbol
    -- as the left sub-tree when the weight is tied with
    -- a Node generated during the building of the tree
    newRoot = if (leftFreq == rightFreq) && (isNothing $ snd leftData)
                 then Node newData rightNode leftNode
                 else Node newData leftNode rightNode

-- Traversal should return a Map of symbols to String
traverseTree node = go node "" Data.Map.empty
  where
    go Empty          _ m = m
    go n@(Node d l r) s m = let m'   = visit n s m
                                m''  = go l (s++"0") m'
                                m''' = go r (s++"1") m''
                            in m'''

-- visit is called above during the tree traversal
visit (Node d Empty Empty) s m = Data.Map.insert k s m
  where 
    k = fromJust $ snd d
-- If not at a leaf node, have nothing to do
visit _ _ m = m
