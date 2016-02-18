
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

-- Now we can write properties

-- Note, though, that these will only have the "Property" type
-- once we apply forAll in the main function. This path allows
-- us to reuse code at least twice for every "property".

prop_fillInCharacter :: (String, Char) -> Bool
prop_fillInCharacter (xs, x) =
    fillInCharacter (freshPuzzle xs) x ==
    Puzzle xs (map (justify x) xs) [x]

prop_checkCharInWord :: Bool -> (String, Char) -> Bool
prop_checkCharInWord b (xs, x) =
    charInWord (Puzzle xs (map (justify x) xs) [x] ) x == b

prop_checkAlreadyGuessed :: Bool -> (String, Char) -> Bool
prop_checkAlreadyGuessed b (xs, x) =
    alreadyGuessed (Puzzle xs (map (justify x) xs) (consIf b x)) x == b
        where 
          consIf :: Bool -> Char -> String
          consIf t c = if t then [c] else []

-- Losing hope? Just print the damn Puzzle

puzzShow :: IO ()
puzzShow = do
  print (fillInCharacter (freshPuzzle "lhs") 'l')

-- OK, run tests

main :: IO ()
main = hspec $ do
  
  describe "Hangman tests -- fillInCharacter" $ do
    it "Guesses the wrong character - does not fill in" $ do
        forAll genMiss  prop_fillInCharacter
    it "Guesses the right character - fills in" $ do
        forAll genMatch prop_fillInCharacter

  describe "Hangman tests -- charInWord" $ do
    it "Checks if Char is in word - returns False" $ do
      forAll genMiss (prop_checkCharInWord False)
    it "Checks if Char is in word - returns True" $ do
      forAll genMatch (prop_checkCharInWord True)

  describe "Hangman tests - alreadyGuessed" $ do
    it "Checks if Char has been guessed (miss) - returns False" $ do
      forAll genMiss (prop_checkAlreadyGuessed False)
    it "Checks the same for a match" $ do
      forAll genMatch (prop_checkAlreadyGuessed False)
      
    it "Checks if Char has been guessed (miss) - returns True" $ do
      forAll genMiss (prop_checkAlreadyGuessed True)
    it "Checks the same for a match" $ do
      forAll genMatch (prop_checkAlreadyGuessed True)
