-- 
-- module Huffman - Functions for dealing with Huffman 
-- trees. This is not a full-fledged Huffman module
-- and has only limited functionality.
-- 
-- This code is part of the Experimental Haskell MP3 Decoder, version 0.0.1.
-- Copyright (c) 2008 Bjorn Edstrom <be@bjrn.se>
--
-- This software is provided 'as-is', without any express or implied
-- warranty. In no event will the authors be held liable for any damages
-- arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
--
--    1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would be
--    appreciated but is not required.
--
--    2. Altered source versions must be plainly marked as such, and must not be
--    misrepresented as being the original software.
--
--    3. This notice may not be removed or altered from any source
--    distribution.
--

module Codec.Audio.MP3.Huffman (
     HuffmanTree(..)
    ,huffmanFromList
    ,huffmanLookupM
) where

data HuffmanTree a = HuffmanNull
                   | HuffmanLeaf a
                   | HuffmanNode (HuffmanTree a) (HuffmanTree a)
                     deriving (Show, Eq)

-- | 'huffmanFromList' builds a tree from a list representation of the tree.
-- The list is a [([Int], t)] where [Int] is a list of code bits and t is the 
-- type of the value associated with the bits. Given the list 
-- [([1],x), ([0,0,1],y), ([0,1],z), ([0,0,0],w)]
-- The function will construct the tree
-- HuffmanNode (HuffmanNode (HuffmanNode (HuffmanLeaf w) 
--                                       (HuffmanLeaf y))
--                          (HuffmanLeaf z)) 
--             (HuffmanLeaf x)
-- This is mainly useful if we have a table representation of a tree,
-- say from a technical specification.
huffmanFromList :: [([Int], t)] -> HuffmanTree t
huffmanFromList = foldl huffmanUpdate HuffmanNull


huffmanUpdate :: HuffmanTree t -> ([Int], t) -> HuffmanTree t
huffmanUpdate HuffmanNull ([], value) = HuffmanLeaf value
huffmanUpdate HuffmanNull (xs, value) = huffmanCreate xs value
    where
        huffmanCreate []     val = HuffmanLeaf val
        huffmanCreate (y:ys) val = 
            if y == 0 then HuffmanNode (huffmanCreate ys val) HuffmanNull
                      else HuffmanNode HuffmanNull (huffmanCreate ys val)
huffmanUpdate (HuffmanNode left right) ((x:xs), value) =
    if x == 0 then HuffmanNode (huffmanUpdate left (xs, value)) right
              else HuffmanNode left (huffmanUpdate right (xs, value))


-- | 'huffmanLookupM' looks up values in a tree. The first argument is a
-- monad action that decides whether to go left or right in the tree.
-- The second argument is the tree. The function returns error or
-- a pair with a value and number if bits consumed.
huffmanLookupM :: (Monad m) => m Bool -> HuffmanTree t -> m (Maybe (t, Int))
huffmanLookupM getbitfunc tree = helper getbitfunc tree 0
    where
        helper _  HuffmanNull         _ = return Nothing
        helper bf (HuffmanNode q0 q1) n = 
            do bit <- bf
               r   <- helper bf (if bit then q1 else q0) (n+1)
               return $ r
        helper _ (HuffmanLeaf leaf)  n = return $ Just (leaf, n)

