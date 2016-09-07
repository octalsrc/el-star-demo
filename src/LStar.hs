{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LStar where

import Data.Map.Strict (Map, member, insert, empty)
import qualified Data.Map.Strict as M
import Control.Monad (foldM)
import Data.Maybe (fromJust)

----------------------------------------------------------------------
-- GENERAL TYPES

class (Eq a, Ord a) => Alphabet a where
  setA :: [a]
  aToChar :: a -> Char
  aFromChar :: Char -> Maybe a

type DFA a = (Map (Int, a) Int, [Int])

----------------------------------------------------------------------
-- TEACHER

class (Monad t, Alphabet a) => Teacher a t where
  memberQ :: [a] -> t Bool
  conjectureQ :: DFA a -> t (Maybe (Prefix a))

----------------------------------------------------------------------
-- LEARNER

type Prefix a = [a]

type Suffix a = [a]

type OTable a = Map (Prefix a, Suffix a) Bool

class Alphabet a => Notes a n where
  getS :: n -> [Prefix a]
  getE :: n -> [Suffix a]
  getT :: n -> OTable a

data Unchecked a = Unchecked { getS' :: [Prefix a]
                             , getE' :: [Suffix a]
                             , getT' :: OTable a   }

initialNotes :: Alphabet a => Unchecked a
initialNotes = undefined

instance Alphabet a => Notes a (Unchecked a) where
  getS = getS'
  getE = getE'
  getT = getT'

rebase :: Notes a n => n -> Unchecked a
rebase n = Unchecked (getS n) (getE n) (getT n)

proven :: (Notes a n1, Notes a n2) => (Unchecked a -> n2) -> n1 -> n2
proven f n = f (rebase n)

extend :: (Teacher a t, Notes a n) 
       => [Prefix a] -> [Suffix a] -> n -> t (Unchecked a)
extend ss es nts =
  let nqs = [ (s,e) | s <- sdota ss, e <- es ]      
      getNewTbl = foldM addEntry (getT nts) nqs
      newSS = (getS nts) ++ ss
      newES = (getE nts) ++ es
  in getNewTbl >>= (\newTbl -> return (Unchecked newSS newES newTbl))

addEntry :: Teacher a t => OTable a -> (Prefix a, Suffix a) -> t (OTable a)
addEntry tbl (s,e) =
  if member (s,e) tbl -- is this already in our table?
     then return tbl -- then we already know this
     else do r <- memberQ (s ++ e) -- then we need to ask the teacher
             return (insert (s,e) r tbl)

-- S `dot` A
sdota :: Alphabet a => [Prefix a] -> [Prefix a]
sdota ss = [ s ++ [a] | s <- ss, a <- setA ]

newtype Consistent a = Consistent (Unchecked a)

instance Alphabet a => Notes a (Consistent a) where
  getS (Consistent u) = getS' u
  getE (Consistent u) = getE' u
  getT (Consistent u) = getT' u

consistent :: (Teacher a t, Notes a n)
           => n
           -> Either (t (Consistent a)) (Consistent a)
consistent n = case consCEs n of
                 (a:_) -> Left (mkConsistent a n)
                 _ -> Right (proven Consistent n)

mkConsistent :: (Teacher a t, Notes a n) 
             => Suffix a
             -> n
             -> t (Consistent a)
mkConsistent e n = do n2 <- extend [] [e] n
                      case consistent n of
                        Left getC -> getC
                        Right c -> return c

tblRow :: Alphabet a => OTable a -> [Suffix a] -> Prefix a -> [Bool]
tblRow t es s = [ fromJust (M.lookup (s,e) t) | e <- es ]

-- counterexamples to consistency
consCEs :: Notes a n => n -> [Suffix a]
consCEs n = let setS = getS n
                setE = getE n
                row = tblRow (getT n) setE
                sc s1 s2 = row s1 == row s2
                ec s1 s2 a e =
                  row (s1 ++ [a] ++ e)
                  /= row (s2 ++ [a] ++ e)
            in [ a:e | s1 <- setS
                     , s2 <- setS
                     , a <- setA
                     , e <- setE
                     , sc s1 s2
                     , ec s1 s2 a e ]

newtype Closed a = Closed (Unchecked a)

instance Alphabet a => Notes a (Closed a) where
  getS (Closed u) = getS' u
  getE (Closed u) = getE' u
  getT (Closed u) = getT' u

closed :: (Teacher a t, Notes a n)
       => n
       -> Either (t (Closed a)) (Closed a)
closed = undefined

newtype CAndC a = CAndC (Unchecked a)

instance Alphabet a => Notes a (CAndC a) where
  getS (CAndC u) = getS' u
  getE (CAndC u) = getE' u
  getT (CAndC u) = getT' u

fromCons :: (Teacher a t)
         => Consistent a
         -> Either (t (Consistent a)) (CAndC a)
fromCons n = case consistent n of
               Right c -> Right (proven CAndC c)
               Left p -> Left p

fromClos :: (Teacher a t)
         => Closed a
         -> Either (t (Closed a)) (CAndC a)
fromClos n = case closed n of
               Right c -> Right (proven CAndC c)
               Left p -> Left p

conjecture :: Teacher a t => CAndC a -> t (Maybe (Prefix a))
conjecture = undefined

conjecture' :: Alphabet a => CAndC a -> DFA a
conjecture' = undefined
