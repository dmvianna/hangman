
module Main where

import Hangman
import Test.Hspec
import Test.QuickCheck

-- Preamble

-- Generate match and miss pairs

alphabet :: String
alphabet = ['a'..'z']

isWord :: String -> Bool
isWord w = foldr (\a b -> a `elem` alphabet && b) True w

genMatch :: Gen ([Char], Char)
genMatch = do
  xs <- arbitrary `suchThat` (\xs -> xs /= [] && isWord xs)
  x <- arbitrary `suchThat` (\x -> x `elem` xs &&  x `elem` alphabet)
  return (xs, x)

genMiss :: Gen ([Char], Char)
genMiss = do
  xs <- arbitrary `suchThat` (\xs -> xs /= [] && isWord xs)
  x <- arbitrary `suchThat`  (\x -> x `notElem` xs && x `elem` alphabet)
  return (xs, x)

-- Properties

-- We need Maybes

justify :: Eq a => a -> a -> Maybe a
justify x y = if x == y
            then Just x
            else Nothing

-- We need an Eq instance for Puzzle

instance Eq Puzzle where
  (==) (Puzzle word discovered guessed)
       (Puzzle word' discovered' guessed') =
    word == word' &&
    discovered == discovered' &&
    guessed == guessed'

prop_PuzzleMiss :: Property
prop_PuzzleMiss =
    forAll genMiss
               (\(xs, x) ->
                    fillInCharacter (freshPuzzle xs) x ==
                    Puzzle xs (map (justify x) xs) [x] )

prop_PuzzleMatch :: Property
prop_PuzzleMatch =
    forAll genMatch
               (\(xs, x) ->
                    fillInCharacter (freshPuzzle xs) x ==
                    Puzzle xs (map (justify x) xs) [x] )

-- Losing hope? Just print the damn Puzzle

puzzShow :: IO ()
puzzShow = do
  print (fillInCharacter (freshPuzzle "lhs") 'l')

-- OK, run tests

main :: IO ()
main = hspec $ do
  describe "Hangman tests -- fillInCharacter" $ do
    it "Guessed the wrong character" $ do
                               prop_PuzzleMiss
    it "Guessed the right character" $ do
                               prop_PuzzleMatch
