{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Monoid ((<>))

import Reflex.Dom

import Automata.DFA
import Automata.LStar

type DFA' = DFA Alphabet'

title :: MonadWidget t m => m ()
title = el "h1" (text "The L* Algorithm")

main :: IO ()
main = mainWidget (do 
  title
  rec b <- yesOrNo t
      el "div" blank
      t <- holdDyn (elstar alphaAB) b
      el "div" blank
      dyn (fmap tableW t)
  blank)

yesOrNo :: MonadWidget t m => Dynamic t FunTeacher' -> m (Event t FunTeacher')
yesOrNo t = do dynText (fmap (T.pack . mqText . funWord) t)
               el "div" blank
               y <- button "Yes"
               n <- button "No"
               let bools = leftmost [const True <$> y
                                    ,const False <$> n]
               return (attachWith (flip funMA) (current t) bools)

mqText "" = "Is the empty string in your language?"
mqText w = "Is the word \"" ++ w ++ "\" in your language?"

-- | A Teacher instance that represents an unanswered question as a
--   function which takes the answer and produces the rest of the
--   questions (while also providing information that might be
--   required to answer the questions)
data FunTeacher l = FunM Alphabet' 
                         ([Prefix Alphabet'],[Suffix Alphabet'],OTable Alphabet')
                         (Prefix Alphabet') 
                         (Suffix Alphabet') 
                         (Bool -> FunTeacher l)
                  | FunQ Alphabet' 
                         Report 
                         (DFA Alphabet') 
                         (Maybe (AWord Alphabet') -> FunTeacher l)
                  | FunR l

type FunTeacher' = FunTeacher (DFA Alphabet')

funWord :: FunTeacher a -> String
funWord (FunM a _ p s _) = map (elemToChar a) (p++s)
funWord _ = "equivalence"

funMA b (FunM _ _ _ _ f) = f b
funMA _ ft = ft

data Report = Report

instance Functor FunTeacher where
  fmap f (FunM a r p s g) = FunM a r p s (fmap f . g)
  fmap f (FunQ a r d g) = FunQ a r d (fmap f . g)
  fmap f (FunR l) = FunR (f l)

instance Applicative FunTeacher where
  pure l = FunR l
  (<*>) (FunR f) v = fmap f v
  (<*>) (FunM a r p s f) v = FunM a r p s ((\f' -> f' <*> v) . f)
  (<*>) (FunQ a r d   f) v = FunQ a r d   ((\f' -> f' <*> v) . f)

instance Monad FunTeacher where
  return = pure
  (>>=) (FunR l) fm = fm l
  (>>=) (FunM a r p s f) fm = FunM a r p s ((\f' -> f' >>= fm) . f)
  (>>=) (FunQ a r d   f) fm = FunQ a r d   ((\f' -> f' >>= fm) . f)

instance Teacher FunTeacher Alphabet' where
  memberQ' a r (p,s) = FunM a r p s (\b -> return b)
  equivQ a dfa = FunQ a Report dfa (\ma -> return ma)
  
funDebug :: Show a => FunTeacher a -> IO ()
funDebug (FunM a _ p s _) = putStrLn ("Asking: " ++ map (elemToChar a) (p++s))
funDebug (FunQ a _ dfa _) = putStrLn ("Asking: " ++ show dfa)
funDebug (FunR l) = putStrLn ("Result: " ++ show l)

ws :: AClass a => a -> AWord a -> String
ws a = map (elemToChar a)

wsp :: AClass a => a -> AWord a -> Text
wsp a = T.pack . ws a

wsp' :: AClass a => a -> AWord a -> Text
wsp' a = T.pack . show . ws a

tableW :: MonadWidget t m => FunTeacher' -> m ()
tableW (FunM a (s,e,t) pr sf _) = elAttr "table" bd (irow >> mapM_ rowW s)
  where rowW pr' = el "tr" (itd >> mapM_ (tdW pr') e)
          where itd = elAttr "td" bd (text (wsp' a pr'))
        tdW pr' sf' = elAttr "td" (stW pr' sf' <> bd) (case M.lookup (pr',sf') t of
                                                         Just True -> text "Y"
                                                         Just False -> text "N"
                                                         Nothing -> text "?")
        stW pr' sf' = if pr' == pr && sf' == sf
                         then M.fromList [("style","background-color: yellow;")]
                         else M.empty
        irow = el "td" blank >> mapM_ (\sf' -> el "td" (text (wsp' a sf'))) e
        bd = M.fromList [("style","border: 1px solid black; border-collapse: collapse;")]
tableW _ = el "p" (text "equiv")
