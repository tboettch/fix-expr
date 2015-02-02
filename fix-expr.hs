
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (and)
import Control.Monad
import Control.Monad.Identity
import Control.Applicative
import Data.Maybe (isJust, isNothing)
import Test.QuickCheck
import GHC.IO (unsafePerformIO)

newtype Fix f = Fix (f (Fix f))

instance Show (f (Fix f)) => Show (Fix f) where
  show (Fix f) = show f

--instance Show f => Show (Fix f) where
--  show (Fix f) = show f

data ExprF f = Lit Value
             | Plus f f
             | And f f
             | If f f f
             deriving (Functor)
             
data Value = NumV Int
           | BoolV Bool
           deriving (Eq)
           
data Type = NumT | BoolT deriving (Eq, Show)
           
instance Show Value where
  show (NumV x) = show x
  show (BoolV x) = show x
             
--instance Functor ExprF where
--  fmap _ (Lit x) = Lit x
--  fmap f (Plus l r) = Plus (f l) (f r)
--  fmap f (And l r) = And (f l) (f r)
--  fmap f (If c l r) = If (f c) (f l) (f r)
               
type Expr = Fix ExprF

instance Show f => Show (ExprF f) where
  show (Lit x) = show x
  show (Plus l r) = parens(show l) ++ " + " ++ parens(show r)
  show (And l r) = parens(show l) ++ " & " ++ parens(show r)
  show (If c t f) = "if(" ++ show c ++ ") then (" ++ show t ++ ") else (" ++ show f ++ ")"
  
parens :: String -> String
parens s = "(" ++ s ++ ")"

num :: Int -> Expr
num x = Fix $ Lit (NumV x)

bool :: Bool -> Expr
bool x = Fix $ Lit (BoolV x)

plus :: Expr -> Expr -> Expr 
plus x y = Fix $ Plus x y

and :: Expr -> Expr -> Expr
and x y = Fix $ And x y

if_ :: Expr -> Expr -> Expr -> Expr
if_ c t f = Fix $ If c t f


typeof :: Expr -> Maybe Type
typeof (Fix (Lit (NumV _))) = Just NumT
typeof (Fix (Lit (BoolV _))) = Just BoolT
typeof (Fix (Plus x y)) = do
  xType <- typeof x
  guard $ xType == NumT
  yType <- typeof y
  guard $ yType == NumT
  return NumT
typeof (Fix (And x y)) = do
  xType <- typeof x
  guard $ xType == BoolT
  yType <- typeof y
  guard $ yType == BoolT
  return BoolT
typeof (Fix (If c t f)) = do
  cType <- typeof c
  guard $ cType == BoolT
  tType <- typeof t
  fType <- typeof f
  guard $ tType == fType
  return tType
  
typecheck :: Expr -> Bool
typecheck = isJust . typeof


eval :: Expr -> Maybe Value
eval (Fix (Lit x)) = Just x
eval (Fix (Plus x y)) = case (eval x, eval y) of
                         (Just (NumV x'), Just (NumV y')) -> Just $ NumV (x' + y')
                         _ -> Nothing
eval (Fix (And x y)) = case (eval x, eval y) of
                         (Just (BoolV x'), Just (BoolV y')) -> Just $ BoolV (x' && y')
                         _ -> Nothing
eval (Fix (If c t f)) = case eval c of
                         (Just (BoolV True)) -> eval t
                         (Just (BoolV False)) -> eval f
                         _ -> Nothing
                         

data AnnotatedF a f = AnnotatedF (a, ExprF f)
type Annotated a = Fix (AnnotatedF a)

instance (Show f, Show a) => Show (AnnotatedF a f) where
  show (AnnotatedF tuple) = show tuple

annotate :: (Expr -> a) -> Expr -> Annotated a
annotate g expr = go expr
  where go e@(Fix x) = let annotation = g e in Fix $ AnnotatedF (annotation, fmap go x)
  

instance Arbitrary Value where
  arbitrary = oneof [fmap NumV arbitrary, fmap BoolV arbitrary]
  shrink (NumV x) = map NumV (shrink x)
  shrink (BoolV x) = map BoolV (shrink x)
  
instance Arbitrary Expr where
  arbitrary = sized arb
    where arb :: Int -> Gen Expr
          arb 0 = fmap (Fix . Lit) arbitrary
          arb d = let nextDepth = choose (0, d - 1)
                      next = nextDepth >>= arb
                  in oneof [ plus <$> next <*> next
                           , and <$> next <*> next
                           , if_ <$> next <*> next <*> next
                           ]                    
  shrink (Fix (Lit x)) = map (Fix . Lit) (shrink x)
  shrink (Fix (Plus l r)) = [l, r] ++  [plus l' r' | (l', r') <- shrink (l, r)]
  shrink (Fix (And l r)) = [l, r] ++  [and l' r' | (l', r') <- shrink (l, r)]
  shrink (Fix (If c t f)) = [c, t, f] ++ [if_ c' t' f' | (c', t', f') <- shrink (c, t, f)]

containsIf :: Expr -> Bool
containsIf (Fix (If _ _ _)) = True
containsIf (Fix (Lit _)) = False
containsIf (Fix (Plus l r)) = containsIf l || containsIf r
containsIf (Fix (And l r)) = containsIf l || containsIf r

prop_typecheck :: Expr -> Property
prop_typecheck e = (label "typechecked" $ typecheck e && isJust (eval e)) 
              .||. (label "didn't typecheck" $ not (containsIf e) ==> not (typecheck e) && isNothing (eval e))
              
prop_typeof :: Expr -> Property              
prop_typeof e = case typeof e of
                  Nothing -> label "didn't typecheck" $ not (containsIf e) ==> isNothing (eval e)
                  (Just NumT) -> label "nums" $ case (eval e) of
                                                 (Just (NumV _)) -> True
                                                 _               -> False
                  (Just BoolT) -> label "bools" $ case (eval e) of
                                                   (Just (BoolV _)) -> True
                                                   _               -> False                                                     

main :: IO ()
main = do
  let args = stdArgs{maxSuccess = 1000, maxSize = 10}
  forM_ [prop_typecheck, prop_typeof] (quickCheckWith args)

