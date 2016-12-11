{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Automata.DFA.Alphabet 
  ( AClass (..)
  , Alphabet (..)

  , AWord
  
  , AChar
  , Alphabet'
  , mkAlphabet
  , alphaToAWord

  ) where

import qualified Data.Set as S
import Data.Set (Set)

-- | An alphabet 'a' with elements 'e'.  These elements must be
-- convertible between 'Char' for practicality.
class AClass a where
  type AElem a
  elements :: (Eq (AElem a), Ord (AElem a)) => a -> Set (AElem a)
  elemToChar :: a -> AElem a -> Char
  charToElem :: a -> Char -> Maybe (AElem a)

-- | Convenience for getting the element type from an alphabet type
-- type family AElem a

-- | A word in the alphabet 'a'
type AWord a = [AElem a]

-- | A value-level 'AClass' instance, for defining alphabets at
-- runtime
newtype Alphabet e = Alphabet (Set e, (e -> Char), (Char -> Maybe e))

-- type instance AElem (Alphabet e) = e

instance (Eq e, Ord e) => AClass (Alphabet e) where
  type AElem (Alphabet e) = e
  elements (Alphabet (es,_,_)) = es
  elemToChar (Alphabet (_,etc,_)) = etc
  charToElem (Alphabet (_,_,cte)) = cte

newtype AChar = AChar { fromAChar :: Char } deriving (Eq, Ord, Read, Show)

-- | The most common 'Alphabet' type anyone will use
type Alphabet' = Alphabet AChar

validate :: [Char] -> Char -> Maybe AChar
validate cs c = if elem c cs
                   then Just (AChar c)
                   else Nothing

-- | Produce an 'Alphabet\'' from a '[Char]'
mkAlphabet :: [Char] -> Alphabet AChar
mkAlphabet cs = Alphabet (S.fromList (map AChar cs), fromAChar, validate cs)

alphaToAWord :: [Char] -> AWord Alphabet'
alphaToAWord = S.toList . elements . mkAlphabet
