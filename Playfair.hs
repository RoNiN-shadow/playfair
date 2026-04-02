module Playfair
( cryptMessage, Mode(..) ) where

import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Data.Char (toUpper)


alphabet :: String
alphabet = filter (/= 'W')['A'..'Z']

formatKey :: String -> String
formatKey (x:xs)
  | x == 'W'  = 'V': formatKey xs 
  | otherwise = toUpper x : formatKey xs
formatKey [] = []

buildGrid :: String -> String
buildGrid key = nub(formatKey key ++ alphabet)

prepareText :: String -> String 
prepareText (a:b:rest)
  | a == b    = a : 'X' : prepareText(b:rest)
  | otherwise = a : b : prepareText rest
prepareText [a] = [a, 'X']
prepareText [] = []

toPairs :: String -> [(Char, Char)]
toPairs (a:b:rest) = (a, b) : toPairs rest

toPairs _ = []

data Mode = Encrypt | Decrypt

processPair :: Mode -> String -> (Char, Char) -> (Char, Char)
processPair mode grid (a, b) =
  let 
      indexA = fromJust (elemIndex a grid)
      indexB = fromJust (elemIndex b grid)

      rowA = indexA `div` 5
      colA = indexA `mod` 5
      rowB = indexB `div` 5
      colB = indexB `mod` 5
  in
      applyRules rowA colA rowB colB

  where
    
    shift = case mode of
            Encrypt -> 1
            Decrypt -> -1
    applyRules rA cA rB cB
      | rA == rB =
          let newColA = (cA + shift) `mod` 5
              newColB = (cB + shift) `mod` 5
          in (getCharByCoords rA newColA, getCharByCoords rB newColB)
      | cA == cB =
          let newRowA = (rA + shift ) `mod` 5
              newRowB = (rB + shift ) `mod` 5
          in (getCharByCoords newRowA cA, getCharByCoords newRowB cB)

      | otherwise = (getCharByCoords rA cB, getCharByCoords rB cA)
    
    getCharByCoords r c = grid !! (r * 5 + c)

pairsToString :: [(Char, Char)] -> String
pairsToString [] = []
pairsToString ((a, b):rest) = a : b : pairsToString rest

cryptMessage :: Mode -> String -> String -> String
cryptMessage mode key message =
  let 
      grid = buildGrid key

      cleanText = prepareText (formatKey message)

      pairs = toPairs cleanText

      processPairs = map (processPair mode grid) pairs

      result = pairsToString processPairs
  in result

