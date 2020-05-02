{-# LANGUAGE OverloadedStrings #-}
module InDyModel where

import Data.Typeable
import Data.List
import Data.Text
import qualified Data.Text as T

------------------------------------------------------    
--   Simulation of ID functional model in Haskell   --
------------------------------------------------------

------------------------------------------------------
--                    Basic things                  --
------------------------------------------------------

-- String contains : Substring -> String -> Bool
op_str_contains :: Text -> String -> Bool
op_str_contains x y = Data.Text.isInfixOf x (Data.Text.pack y)

-- String contains : String -> Substring -> Bool
flip_op_str_contains :: String -> Text -> Bool
flip_op_str_contains = flip op_str_contains

-- Operator k (constant): return copy of the first argument
op_k :: a -> b -> a
op_k x y = x

-- Operator False: Any -> Bool
op_false :: a -> Bool
op_false x = op_k False x

-- Operator True: Any -> Bool
op_true :: a -> Bool
op_true x = op_k True x

-- Arithmetic addition
op_add :: (Num a) => a -> a -> a
op_add a b = a + b

-- Equals
op_equals :: (Eq a) => a -> a -> Bool
op_equals a b = a == b

-- Bool disjunction
op_disj :: Bool -> Bool -> Bool
op_disj a b = a || b

-- Choice operator
choice :: Bool -> a -> a -> a
choice x a b = if x then a else b

-- Identity operator
op_id :: a -> a
op_id a = a

------------------------------------------------------
--                    List things                   --
------------------------------------------------------

-- List contains predicate : List -> Operator -> Bool
op_list_contains_p :: [a] -> (a -> Bool) -> Bool
op_list_contains_p x op = Data.List.any op x

-- Filter list by predicate : Operator -> List -> List
op_filter :: (a -> Bool) -> [a] -> [a]
op_filter = Data.List.filter

-- Map : Operator -> List -> List
op_map :: (a -> b) -> [a] -> [b]
op_map f x = Data.List.map f x

-- Map flipped: List -> Operator -> List
flip_op_map :: [a] -> (a->b) -> [b]
flip_op_map = flip op_map

-- Reduce: Operator -> List -> Any -> Any
op_reduce :: (a -> b -> b) -> [a] -> b -> b
op_reduce op seq val = Data.List.foldr op val seq

-- Split on operator
split_on :: Text -> Text -> [String]
split_on d s = list_map T.unpack (T.splitOn d s)


------------------------------------------------------
--                    Operator things               --
------------------------------------------------------

-- Apply: Operator -> Any -> Any
op_apply :: (a -> b) -> a -> b
op_apply f a = f(a)

-- Apply flipped
flip_op_apply :: a -> (a -> b) -> b
flip_op_apply = flip op_apply

-- Apply 2 arguments
op_apply2 :: (a -> b -> c) -> a -> b -> c
op_apply2 op a b = op a b

-- Apply 3 arguments
op_apply3 :: (a -> b -> c -> d) -> a -> b -> c -> d
op_apply3 op a b c = op a b c

-- Operator disjunction
op_disj_op :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
op_disj_op x y a = (x a) || (y a)

-- Pipe or function composition
pipe :: (a -> b) -> (b -> c) -> (a -> c)
pipe f1 f2 x = f2 (f1 x)

-- Pipe 2 arguments
-- gives its input to the first and second operators, 
-- and pipes the outputs from both of them to the third operator.
pipe2 :: (a -> b) -> (a -> c) -> (b -> c -> d) -> a -> d
pipe2 op1 op2 op3 x = op3 (op1 x) (op2 x)


-- Names for direct usage
reduce   = op_reduce
apply    = op_apply
apply2   = op_apply2
apply3   = op_apply3
list_map = Data.List.map

