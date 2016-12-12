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

import Control.Monad.Reader
import Control.Monad.State

import Automata.DFA

----------------------------------------------------------------------
-- TEACHER

type Prefix a = AWord a
type Suffix a = AWord a

class (AClass a) => Teacher t a where
  memberQ :: a -> (Prefix a, Suffix a) -> t Bool
  equivQ :: a -> DFA a -> t (Maybe (AWord a))

newtype TestTeacher a = TestTeacher a deriving (Read, Show, Eq, Ord)

instance Functor TestTeacher where
  fmap f (TestTeacher a) = TestTeacher (f a)

instance Teacher TestTeacher Alphabet' where
  memberQ a (p,s) = TestTeacher (if [] == (p ++ s)
                                    then True
                                    else False)
  equivQ _ _ = TestTeacher Nothing
  
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
  equivQ a dfa = AskTeacher (askDFA a dfa)
  
askDFA a dfa = 
  do putStrLn ("Is this DFA correct?")
     print dfa
     r <- getLine
     case r of
       "y" -> return Nothing
       "n" -> do putStrLn ("Please provide a counterexample:")
                 ce <- getLine
                 return (mkAWord a ce)
       _ -> putStrLn "I didn't understand that..."
            >> askDFA a dfa

askWord :: (AClass a) => a -> AWord a -> IO Bool
askWord a w = do putStrLn ("Is the word \"" 
                           ++ (map (elemToChar a) w)
                           ++ "\" in the language? ('y' or 'n')")
                 r <- getLine
                 case r of
                   "y" -> return True
                   "n" -> return False
                   _ -> putStrLn "I didn't understand that..."
                        >> askWord a w

askDebug :: (Show a) => AskTeacher (a) -> IO ()
askDebug (AskTeacher ioa) = ioa >>= print

----------------------------------------------------------------------
-- LEARNER

type LState t a = StateT (Pos a,[Pos a]) (ReaderT a t)

data Pos a = PosUnf (Unfilled a)
           | PosUnc (Unchecked a)
           | PosClo (Closed a)

type OTable a = Map (Prefix a, Suffix a) Bool

class Notes n where
  getS :: AClass a => n a -> [Prefix a]
  getE :: AClass a => n a -> [Suffix a]
  getT :: AClass a => n a -> OTable a

data Unfilled a = Unfilled { getS' :: [Prefix a]
                           , getE' :: [Suffix a]
                           , getT' :: OTable a }

deriving instance (Ord (AElem a), Read (AElem a)) => Read (Unfilled a)
deriving instance Show (AElem a) => Show (Unfilled a)
deriving instance Eq (AElem a) => Eq (Unfilled a)
deriving instance Ord (AElem a) => Ord (Unfilled a)

instance Notes Unfilled where
  getS = getS'
  getE = getE'
  getT = getT'

newtype Unchecked a = Unchecked { fromUnchecked :: Unfilled a }

deriving instance (Ord (AElem a), Read (AElem a)) => Read (Unchecked a)
deriving instance Show (AElem a) => Show (Unchecked a)
deriving instance Eq (AElem a) => Eq (Unchecked a)
deriving instance Ord (AElem a) => Ord (Unchecked a)

instance Notes Unchecked where
  getS = getS . fromUnchecked
  getE = getE . fromUnchecked
  getT = getT . fromUnchecked

newtype Closed a = Closed { fromClosed :: Unchecked a } 

deriving instance (Ord (AElem a), Read (AElem a)) => Read (Closed a)
deriving instance Show (AElem a) => Show (Closed a)
deriving instance Eq (AElem a) => Eq (Closed a)
deriving instance Ord (AElem a) => Ord (Closed a)

instance Notes Closed where
  getS (Closed n) = getS n
  getE (Closed n) = getE n
  getT (Closed n) = getT n

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
                 in Unchecked <$> Unfilled newS e <$> newTable

findEntry :: (Monad t, Teacher t a) 
          => a -> OTable a -> (Prefix a, Suffix a) -> t (OTable a)
findEntry a t w = case M.lookup w t of
                    Nothing -> case copyEntry a t w of
                                 Just t' -> return t'
                                 Nothing -> addEntry a w t
                    _ -> return t

copyEntry :: AClass a 
          => a -> OTable a -> (Prefix a, Suffix a) -> Maybe (OTable a)
copyEntry a t (p,s) = 
  fmap (\(_,v) -> M.insert (p,s) v t) 
       (L.find (\((p',s'),v) -> p ++ s == p' ++ s') (M.toList t))

extendE :: (Monad t, Teacher t a, Notes n) 
        => a -> [Suffix a] -> n a -> t (Unchecked a)
extendE a ss n = let (s,e,t) = (getS n, getE n, getT n)
                     newE = e ++ ss
                     newEntries = [(pr,sf) | pr <- (allPs a s), sf <- newE]
                     newTable = foldM (findEntry a) t newEntries
                 in Unchecked <$> Unfilled s newE <$> newTable

-- TODO: combine extendE and extendS into one function

initNotes :: (Monad t, Teacher t a) => a -> t (Unchecked a)
initNotes a = 
  return (Unchecked $ Unfilled [] [] empty)
  >>= extendE a ([eps a] ++ (map (\e -> [e]) . S.toList . elements $ a))
  >>= extendS a [eps a]

checkClosed :: (AClass a) => a -> Unchecked a -> Either (Prefix a) (Closed a)
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
           => a -> Unchecked a -> t (Closed a)
makeClosed a n = 
  case checkClosed a n of
    Left p -> extendS a [p] n >>= makeClosed a
    Right c -> return c

conjecture :: (AClass a) => a -> Closed a -> DFA a
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
processCE a n w = 
  let (s,e,t) = (getS n, getE n, getT n)
      orEmpty (Just s) = s
      orEmpty Nothing = eps a

      lpf = orEmpty
            . listToMaybe 
            . L.sortBy (\a b -> compare (length b) (length a))
            . filter (L.isPrefixOf w)
            $ allPs a s
      sfs = fmap (suffixCl (not . (`elem` e))) 
            . ((flip L.stripPrefix) w)
            $ lpf
  in extendE a (fromJust sfs) n

suffixCl :: ([a] -> Bool) -> [a] -> [[a]]
suffixCl p as = 
  let as' = reverse as
  in filter p 
     . map reverse 
     . map ((flip take) as') 
     $ [1 .. (length as')]

elstar :: (Monad t, Teacher t a) => a -> t (DFA a)
elstar a = initNotes a >>= starLoop a

starLoop :: (Monad t, Teacher t a) => a -> Unchecked a -> t (DFA a)
starLoop a n = 
  do cl <- makeClosed a n
     let dfa = conjecture a cl
     res <- equivQ a dfa
     case res of
       Just ce -> processCE a cl ce >>= starLoop a
       Nothing -> return dfa
