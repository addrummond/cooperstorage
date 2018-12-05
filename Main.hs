{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
module Main where

main = return ()

data Individual = John | Tom | Bill | Jane

class HasTrace a where
    type TraceOf a :: *

instance HasTrace ((Individual -> Bool) -> Bool) where
    type TraceOf ((Individual -> Bool) -> Bool) = Individual

data BinTree a = Empty | Leaf a | Branch (BinTree a) (BinTree a)

data List a = Nil | Cons a (List a)

data HList (l::List *) where
    HNil  :: HList Nil
    HCons :: e -> HList l -> HList ('Cons e l)

type family Concat (a::k) (b::k) :: k where
    Concat 'Nil a = a
    Concat ('Cons a xs) ys = 'Cons a (Concat xs ys)

hListConcat :: HList a -> HList b -> HList (Concat a b)
hListConcat HNil xs = xs
hListConcat (HCons a xs) ys = HCons a (hListConcat xs ys)

type family Rest (a::k) :: k where
    Rest 'Nil = 'Nil
    Rest ('Cons _ xs) = xs

hListRest :: HList a -> HList (Rest a)
hListRest (HCons a xs) = xs

type family First a :: * where
    First 'Nil = ()
    First ('Cons a _) = a

hListFirst :: HList a -> First a
hListFirst HNil = ()
hListFirst (HCons a xs) = a

data Val s a = Val s a

store
    :: Val (HList 'Nil) (HList 'Nil -> a)
    -> Val (HList ('Cons a 'Nil)) (HList ('Cons (TraceOf a) 'Nil) -> TraceOf a)
store (Val store a) =
    Val (HCons (a HNil) HNil) (\(HCons x _) -> x)

retrieve
    :: Val (HList ('Cons ((a -> b) -> c) store)) (HList ('Cons a params) -> b)
       -> Val (HList store) (HList params -> c)
retrieve (Val store v) =
    Val (hListRest store)
        (\ps -> stored (\x -> (v (HCons x ps))))
    where
        stored = hListFirst store

hListSplit
    :: HList a -> HList b -> HList (Concat a b) -> (HList a, HList b)
hListSplit HNil HNil HNil = (HNil, HNil)
hListSplit (HCons a xs) ys (HCons c zs) =
    (HCons c f, s)
    where
        (f, s) = hListSplit xs ys zs
hListSplit HNil _ zs = (HNil, zs)

apply
    :: Val (HList store1) (HList params1 -> a -> b)
    -> Val (HList store2) (HList params2 -> a)
    -> Val (HList (Concat store1 store2)) (HList (Concat params1 params2) -> b)
apply (Val store1 f1) (Val store2 f2) =
    Val (hListConcat store1 store2)
        (\ps ->
            let
                (p1, p2) = hListSplit p1 p2 ps
            in
                (f1 p1) (f2 p2)
        )

every :: Val (HList 'Nil) (HList 'Nil -> (Individual -> Bool) -> Bool)
every = Val HNil (\_ _ -> True)

smokes :: Val (HList 'Nil) (HList 'Nil -> Individual -> Bool)
smokes = Val HNil (\_ _ -> True)

likes :: Val (HList 'Nil) (HList 'Nil -> Individual -> Individual -> Bool)
likes = Val HNil (\_ _ _ -> True)

john :: Val (HList 'Nil) (HList 'Nil -> Individual)
john = Val HNil (\_ -> John)

test = retrieve (apply (apply likes (store every)) john)

run :: Val (HList 'Nil) (HList 'Nil -> a) -> a
run (Val _ f) = f HNil
