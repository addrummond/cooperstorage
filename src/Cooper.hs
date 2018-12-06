{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
module Cooper
    ( HasTrace(..)
    , Simple
    , Val(..)
    , (<|)
    , (|>)
    , apply
    , lift
    , retrieve
    , run
    , store
    ) where

import HList (HList(HCons, HNil), HListConcat, HListSplit, List(Cons, Nil), hListConcat, hListFirst, hListRest, hListSplit)

class HasTrace a where
    type TraceOf a :: *

data Val s a where
    Val :: HList s -> (HList a -> b) -> Val (HList s) ((HList a) -> b)

-- a simple value with an empty store (and hence no parameters)
type Simple a = Val (HList 'Nil) (HList 'Nil -> a)
 
lift :: a -> Val (HList 'Nil) (HList 'Nil -> a)
lift v = Val HNil (\_ -> v)

store
    :: Val (HList 'Nil) (HList 'Nil -> a)
    -> Val (HList ('Cons a 'Nil)) (HList ('Cons (TraceOf a) 'Nil) -> TraceOf a)
store (Val store a) =
    Val (HCons (a HNil) HNil) (\(HCons x _) -> x)

-- retrieves and applies the first value in the store
retrieve
    :: Val (HList ('Cons ((a -> b) -> c) store)) (HList ('Cons a params) -> b)
    -> Val (HList store) (HList params -> c)
retrieve (Val store v) =
    Val (hListRest store)
        (\ps -> stored (\x -> (v (HCons x ps))))
    where
        stored = hListFirst store

apply
    :: HListSplit params1 params2
    => Val (HList store1) (HList params1 -> a -> b)
    -> Val (HList store2) (HList params2 -> a)
    -> Val (HList (HListConcat store1 store2)) (HList (HListConcat params1 params2) -> b)
apply (Val store1 f1) (Val store2 f2) =
    Val (hListConcat store1 store2)
        (\ps ->
            let
                (p1, p2) = hListSplit ps
            in
                (f1 p1) (f2 p2)
        )

-- operator synonym for 'apply'
(<|)
    :: HListSplit params1 params2
    => Val (HList store1) (HList params1 -> a -> b)
    -> Val (HList store2) (HList params2 -> a)
    -> Val (HList (HListConcat store1 store2)) (HList (HListConcat params1 params2) -> b)
(<|) = apply

-- operator synoym for 'apply' that takes function on the right
(|>)
    :: HListSplit params1 params2
    => Val (HList store2) (HList params2 -> a)
    -> Val (HList store1) (HList params1 -> a -> b)
    -> Val (HList (HListConcat store1 store2)) (HList (HListConcat params1 params2) -> b)
(|>) = flip apply

infixr 1 <|
infixl 1 |>

run :: Val (HList 'Nil) (HList 'Nil -> a) -> a
run (Val _ f) = f HNil