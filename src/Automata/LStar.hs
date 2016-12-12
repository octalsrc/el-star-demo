{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Automata.LStar where

import Data.Maybe
import qualified Data.List as L
import Data.Map.Strict (Map, member, insert, empty)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.List (nub)

import Automata.DFA

----------------------------------------------------------------------
-- TEACHER

type Prefix a = AWord a
type Suffix a = AWord a

class (AClass a) => Teacher t a where
  memberQ :: a -> (Prefix a, Suffix a) -> t Bool
  conjectureQ :: a -> DFA a -> t (Maybe (AWord a))

newtype TestTeacher a = TestTeacher a deriving (Read, Show, Eq, Ord)

instance Functor TestTeacher where
  fmap f (TestTeacher a) = TestTeacher (f a)

instance Teacher TestTeacher Alphabet' where
  memberQ a (p,s) = TestTeacher (if [] == (p ++ s)
                                    then True
                                    else False)
  conjectureQ _ _ = TestTeacher Nothing
  
testQ = case (mkAWord exampleAlphabet "", mkAWord exampleAlphabet "") of
          (Just pr, Just sf) -> Just (pr, sf)
          _ -> Nothing

testResponse :: Maybe (TestTeacher Bool)
testResponse = memberQ exampleAlphabet <$> testQ

newtype AskTeacher a = AskTeacher { fromAskTeacher :: IO a }

-- TODO: try replacing these instances with generalized deriving

instance Functor AskTeacher where
  fmap f (AskTeacher ioa) = AskTeacher (fmap f ioa)

instance Applicative AskTeacher where
  pure a = AskTeacher (pure a)
  (<*>) (AskTeacher iof) (AskTeacher a) = AskTeacher (iof <*> a)

instance Monad AskTeacher where
  return = pure
  (>>=) (AskTeacher ioa) f = AskTeacher (ioa >>= (fromAskTeacher . f))

instance Teacher AskTeacher Alphabet' where
  memberQ a (p,s) = AskTeacher (askWord a (p ++ s))
  conjectureQ = undefined

askWord :: (AClass a) => a -> AWord a -> IO Bool
askWord a w = do putStrLn ("Is the word \"" 
                           ++ (map (elemToChar a) w)
                           ++ "\" in the language? ('True' or 'False')")
                 read <$> getLine

askDebug :: (Show a) => AskTeacher (a) -> IO ()
askDebug (AskTeacher ioa) = ioa >>= print

----------------------------------------------------------------------
-- LEARNER

type OTable a = Map (Prefix a, Suffix a) Bool

class Notes n where
  getS :: AClass a => n a -> [Prefix a]
  getE :: AClass a => n a -> [Suffix a]
  getT :: AClass a => n a -> OTable a

data Unchecked a = Unchecked { getS' :: [Prefix a]
                             , getE' :: [Suffix a]
                             , getT' :: OTable a }
                             
deriving instance Show (AElem a) => Show (Unchecked a)


instance Notes Unchecked where
  getS = getS'
  getE = getE'
  getT = getT'

addEntry :: (Functor t, Teacher t a) 
         => a -> (Prefix a, Suffix a) -> OTable a -> t (OTable a)
addEntry a (p,s) t = fmap (\v -> insert (p,s) v t) (memberQ a (p,s))

addEntry' a t w = addEntry a w t


extraPs :: AClass a => a -> [Prefix a] -> [Prefix a]
extraPs a s = let as = S.toList $ elements a
                  cross = [pr ++ [e] | pr <- s, e <- as]
              in filter (not . (flip elem) s) cross

allPs :: AClass a => a -> [Prefix a] -> [Prefix a]
allPs a s = s ++ extraPs a s

extendS :: (Monad t, Teacher t a, Notes n) 
        => a -> [Prefix a] -> n a -> t (Unchecked a)
extendS a ps n = let (s,e,t) = (getS n, getE n, getT n)
                     newS = s ++ ps
                     newEntries = [(pr,sf) | pr <- (allPs a newS), sf <- e]
                     newTable = foldM (findEntry a) t newEntries
                 in Unchecked newS e <$> newTable

findEntry :: (Monad t, Teacher t a) 
          => a -> OTable a -> (Prefix a, Suffix a) -> t (OTable a)
findEntry a t w = case M.lookup w t of
                    Nothing -> addEntry a w t
                    _ -> return t

extendE :: (Monad t, Teacher t a, Notes n) 
        => a -> [Suffix a] -> n a -> t (Unchecked a)
extendE a ss n = let (s,e,t) = (getS n, getE n, getT n)
                     newE = e ++ ss
                     newEntries = [(pr,sf) | pr <- (allPs a s), sf <- newE]
                     newTable = foldM (findEntry a) t newEntries
                 in Unchecked s newE <$> newTable

-- TODO: combine extendE and extendS into one function

initNotes :: (Monad t, Teacher t a) => a -> t (Unchecked a)
initNotes a = 
  return (Unchecked [] [] empty) 
  >>= extendE a ([eps a] ++ (map (\e -> [e]) . S.toList . elements $ a))
  >>= extendS a [eps a]

newtype Closed n a = Closed (n a) deriving (Read, Show, Eq, Ord)

instance (Notes n) => Notes (Closed n) where
  getS (Closed n) = getS n
  getE (Closed n) = getE n
  getT (Closed n) = getT n

checkClosed :: (AClass a, Notes n) => a -> n a -> Either (Prefix a) (Closed n a)
checkClosed a n = let (s,e,t) = (getS n, getE n, getT n)
                      getRow' = getRow a t e 
                      srows = map getRow' s
                      ps = filter (\p -> not (getRow' p `elem` srows))
                                  (allPs a s)
                  in case listToMaybe ps of
                       Just p -> Left p
                       Nothing -> Right (Closed n)

getRow :: (AClass a) => a -> OTable a -> [Suffix a] -> Prefix a -> [Bool]
getRow _ t ss p = map (\s -> t M.! (p,s)) ss

makeClosed :: (Monad t, Teacher t a) 
           => a -> Unchecked a -> t (Closed Unchecked a)
makeClosed a n = 
  case checkClosed a n of
    Left p -> extendS a [p] n >>= makeClosed a
    Right c -> return c

conjecture :: (AClass a, Notes n) => a -> Closed n a -> DFA a
conjecture a n = 
  let (s,e,t) = (getS n, getE n, getT n)
      getRow' = getRow a t e

      states = zip (map getRow' s) [1..]
      findState w = M.fromList states M.! getRow' w

      revs = map (\p -> (getRow' p, p)) s
      revPre w = M.fromList revs M.! w

      istate = findState (eps a)
      tr (rw,st) e = (st,(findState . (++ [e]) . revPre) rw,e)
      rstates = map (\(rw,st) -> (st,t M.! (revPre rw,eps a))) 
                    states
      trans = [tr st e | st <- states, e <- (S.toList . elements) a]
  in mkDFA a rstates istate trans

processCE :: (Monad t, Teacher t a, Notes n) 
          => a -> n a -> AWord a -> t (Unchecked a)
processCE = undefined

-- extend :: (Teacher t a) => [Prefix a] -> [Suffix a] -> Unchecked a -> t (Unchecked a)
-- extend ps ss (Unchecked s e t) = fill (s ++ ps) (e ++ ss) t

-- fill :: (Teacher t a) => a -> [Prefix a] -> [Suffix a] -> OTable a -> t (Unchecked a)
-- fill a ps ss t = let vs = mapM (\p -> memberQ a p) ps
--                      tbl = foldr insert t <$> vs
--                  in undefined

-- initNotesW :: Alphabet a => a -> Unchecked a
-- initNotesW _ = Unchecked [] [] empty

-- initialNotes :: (Teacher a t) => a -> t (Unchecked a)
-- initialNotes as = extend [eps] [eps] (initNotesW as)


-- rebase :: Notes a n => n -> Unchecked a
-- rebase n = Unchecked (getS n) (getE n) (getT n)

-- proven :: (Notes a n1, Notes a n2) => (Unchecked a -> n2) -> n1 -> n2
-- proven f n = f (rebase n)

-- extend :: (Teacher a t, Notes a n) 
--        => [Prefix a] -> [Suffix a] -> n -> t (Unchecked a)
-- extend ss es nts =
--   let nqs = [ (s,e) | s <- sdota ss, e <- es ]      
--       getNewTbl = foldM addEntry (getT nts) nqs
--       newSS = (getS nts) ++ ss
--       newES = (getE nts) ++ es
--   in getNewTbl >>= (\newTbl -> return (Unchecked newSS newES newTbl))

-- addEntry :: Teacher a t => OTable a -> (Prefix a, Suffix a) -> t (OTable a)
-- addEntry tbl (s,e) =
--   if member (s,e) tbl -- is this already in our table?
--      then return tbl -- then we already know this
--      else do r <- memberQ (s <> e) -- then we need to ask the teacher
--              return (insert (s,e) r tbl)

-- -- S `dot` A
-- sdota :: Alphabet a => [Prefix a] -> [Prefix a]
-- sdota ss = [ app s a | s <- ss, a <- setA ]

-- newtype Consistent a = Consistent (Unchecked a)

-- instance Alphabet a => Notes a (Consistent a) where
--   getS (Consistent u) = getS' u
--   getE (Consistent u) = getE' u
--   getT (Consistent u) = getT' u

-- consistent :: (Teacher a t, Notes a n)
--            => n
--            -> Either (t (Consistent a)) (Consistent a)
-- consistent n = case consCEs n of
--                  (a:_) -> Left (mkConsistent a n)
--                  _ -> Right (proven Consistent n)

-- mkConsistent :: (Teacher a t, Notes a n) 
--              => Suffix a
--              -> n
--              -> t (Consistent a)
-- mkConsistent e n = do n2 <- extend [] [e] n
--                       case consistent n of
--                         Left getC -> getC
--                         Right c -> return c

-- tblRow :: Alphabet a => OTable a -> [Suffix a] -> Prefix a -> [Bool]
-- tblRow t es s = [ fromJust (M.lookup (s,e) t) | e <- es ]

-- -- counterexamples to consistency
-- consCEs :: Notes a n => n -> [Suffix a]
-- consCEs n = let setS = getS n
--                 setE = getE n
--                 row = tblRow (getT n) setE
--                 sc s1 s2 = row s1 == row s2
--                 ec s1 s2 a e =
--                   row (app s1 a <> e)
--                   /= row (app s2 a <> e)
--             in [ str a <> e | s1 <- setS
--                             , s2 <- setS
--                             , a <- setA
--                             , e <- setE
--                             , sc s1 s2
--                             , ec s1 s2 a e ]

-- newtype Closed a = Closed (Unchecked a)

-- instance Alphabet a => Notes a (Closed a) where
--   getS (Closed u) = getS' u
--   getE (Closed u) = getE' u
--   getT (Closed u) = getT' u

-- closed :: (Teacher a t, Notes a n)
--        => n
--        -> Either (t (Closed a)) (Closed a)
-- closed n = case closCEs n  of
--              a:_ -> Left (mkClosed a n)
--              _ -> Right (proven Closed n)

-- closCEs :: Notes a n => n -> [Prefix a]
-- closCEs n = let setS = getS n
--                 setE = getE n
--                 row = tblRow (getT n) setE
--                 rows = map row setS
--             in [ app s a | s <- setS
--                          , a <- setA
--                          , not (elem (row (app s a)) rows)]

-- mkClosed :: (Teacher a t, Notes a n) 
--          => Prefix a
--          -> n
--          -> t (Closed a)
-- mkClosed p n = do n2 <- extend [p] [] n
--                   case closed n of
--                     Left getC -> getC
--                     Right c -> return c

-- newtype CAndC a = CAndC (Unchecked a)

-- instance Alphabet a => Notes a (CAndC a) where
--   getS (CAndC u) = getS' u
--   getE (CAndC u) = getE' u
--   getT (CAndC u) = getT' u

-- fromCons :: (Teacher a t)
--          => Consistent a
--          -> Either (t (Closed a)) (CAndC a)
-- fromCons n = case closed n of
--                Right c -> Right (proven CAndC c)
--                Left p -> Left p

-- fromClos :: (Teacher a t)
--          => Closed a
--          -> Either (t (Consistent a)) (CAndC a)
-- fromClos n = case consistent n of
--                Right c -> Right (proven CAndC c)
--                Left p -> Left p

-- conjecture :: Teacher a t => CAndC a -> t (Maybe (Prefix a))
-- conjecture = undefined

-- conjecture' :: Alphabet a => [a] -> CAndC a -> DFA a
-- conjecture' as c = let setS = getSw as c
--                        setE = getEw as c
--                        tbl = getTw as c
--                        row = tblRow tbl setE
--                        setQ = nub [ row s | s <- setS ]
--                        qZ = row eps
--                        -- setF ?
--                    in (M.fromList (zip (zip (setQ) as) setQ), [[]])

-- -- type DFA a = (Map ([Bool], a) [Bool], [[Bool]])

-- elstar :: Teacher a t => [a] -> t (DFA a)
-- elstar as = undefined
