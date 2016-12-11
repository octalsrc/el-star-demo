{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module LStar.Alphabet 
  ( AClass (..)
  , Alphabet (..)
  
  , AElem
  , AWord
  
  , Alphabet'
  , mkAlphabet
  
  ) where

import qualified Data.Set as S
import Data.Set (Set)

-- | An alphabet 'a' with elements 'e'.  These elements must be
-- convertible between 'Char' for practicality.
class (Eq e, Ord e) => AClass a e | a -> e where
  elements :: a -> Set e
  elemToChar :: a -> e -> Char
  charToElem :: a -> Char -> Maybe e

-- | Convenience for getting the element type from an alphabet type
type family AElem a

-- | A word in the alphabet 'a'
type AWord a = [AElem a]

-- | A value-level 'AClass' instance, for defining alphabets at
-- runtime
newtype Alphabet e = Alphabet (Set e, (e -> Char), (Char -> Maybe e))

type instance AElem (Alphabet e) = e

instance (Eq e, Ord e) => AClass (Alphabet e) e where
  elements (Alphabet (es,_,_)) = es
  elemToChar (Alphabet (_,etc,_)) = etc
  charToElem (Alphabet (_,_,cte)) = cte

-- | The most common 'Alphabet' type anyone will use
type Alphabet' = Alphabet Char

-- | Produce an 'Alphabet\'' from a '[Char]'
mkAlphabet :: [Char] -> Alphabet'
mkAlphabet cs = Alphabet (S.fromList cs, id, return)

alphaToAWord :: [Char] -> AWord Alphabet'
alphaToAWord = S.toList . elements . mkAlphabet
