{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
module Cooper
    ( ComposeWith(..)
    , HasTrace(..)
    , Simple
    , Val(..)
    , (<|)
    , (|>)
    , apply
    , lift
    , retrieve
    , retrieve2
    , retrieve3
    , retrieve4
    , run
    , store
    ) where

import HList (HList(HCons, HNil), HListConcat, HListSplit, List(Cons, Nil), hListConcat, hListFirst, hListRest, hListSplit)

class HasTrace a where
    type TraceOf a :: *

class ComposeWith a b c | a b -> c where
    composeWith :: a -> b -> c

-- composition by function application
instance ComposeWith (a -> b) a b where
    composeWith = id

-- composition by predicate modification
instance ComposeWith (a -> Bool) (a -> Bool) (a -> Bool) where
    composeWith p1 p2 x = p1 x && p2 x

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
    :: (HListSplit params1 params2, ComposeWith f a r)
    => Val (HList store1) (HList params1 -> f)
    -> Val (HList store2) (HList params2 -> a)
    -> Val (HList (HListConcat store1 store2)) (HList (HListConcat params1 params2) -> r)
apply (Val store1 f1) (Val store2 f2) =
    Val (hListConcat store1 store2)
        (\ps ->
            let
                (p1, p2) = hListSplit ps
            in
                composeWith (f1 p1) (f2 p2)
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

-- retrieves and applies the second value in the store
retrieve2
    :: Val (HList ('Cons sfst ('Cons ((a -> b) -> c) store))) (HList ('Cons pfst ('Cons a params)) -> b)
    -> Val (HList ('Cons sfst store)) (HList ('Cons pfst params) -> c)
retrieve2 (Val store v) =
    Val (HCons (hListFirst store) (hListRest (hListRest store)))
        (\ps -> stored (\x -> (v (HCons (hListFirst ps) (HCons x (hListRest ps))))))
    where
        stored = hListFirst (hListRest store)

-- retrieves and applies the third value in the store
retrieve3
    :: Val (HList ('Cons sfst ('Cons ssnd ('Cons ((a -> b) -> c) store)))) (HList ('Cons pfst ('Cons psnd ('Cons a params))) -> b)
    -> Val (HList ('Cons sfst ('Cons ssnd store))) (HList ('Cons pfst ('Cons psnd params)) -> c)
retrieve3 (Val store v) =
    Val (HCons (hListFirst store) (HCons (hListFirst (hListRest store)) (hListRest  (hListRest (hListRest store)))))
        (\ps -> stored (\x -> (v (HCons (hListFirst ps) (HCons (hListFirst (hListRest ps)) (HCons x (hListRest (hListRest ps))))))))
    where
        stored = hListFirst (hListRest (hListRest store))

-- retrieves and applies the fourth value in the store
retrieve4
    :: Val (HList ('Cons sfst ('Cons ssnd ('Cons strd ('Cons ((a -> b) -> c) store))))) (HList ('Cons pfst ('Cons psnd ('Cons ptrd ('Cons a params)))) -> b)
    -> Val (HList ('Cons sfst ('Cons ssnd ('Cons strd store)))) (HList ('Cons pfst ('Cons psnd ('Cons ptrd params))) -> c)
retrieve4 (Val store v) =
    Val (HCons (hListFirst store) (HCons (hListFirst (hListRest store)) (HCons (hListFirst (hListRest (hListRest store))) (hListRest (hListRest (hListRest (hListRest store)))))))
        (\ps -> stored (\x -> (v (HCons (hListFirst ps) (HCons (hListFirst (hListRest ps)) (HCons (hListFirst (hListRest (hListRest ps))) (HCons x (hListRest (hListRest (hListRest ps))))))))))
    where
        stored = hListFirst (hListRest (hListRest (hListRest store)))