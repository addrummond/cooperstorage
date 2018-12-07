{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
module HList
    ( HList(..)
    , HListConcat
    , HListFirst
    , HListSplit
    , HListRest
    , List(..)
    , hListConcat
    , hListFirst
    , hListRest
    , hListSplit
    ) where

data List a = Nil | Cons a (List a)

data HList (l::List *) where
    HNil  :: HList Nil
    HCons :: e -> HList l -> HList ('Cons e l)

type family HListConcat (a::k) (b::k) :: k where
    HListConcat 'Nil a = a
    HListConcat ('Cons a xs) ys = 'Cons a (HListConcat xs ys)

hListConcat :: HList a -> HList b -> HList (HListConcat a b)
hListConcat HNil xs = xs
hListConcat (HCons a xs) ys = HCons a (hListConcat xs ys)

type family HListRest (a::k) :: k where
    HListRest 'Nil = 'Nil
    HListRest ('Cons _ xs) = xs

hListRest :: HList a -> HList (HListRest a)
hListRest (HCons a xs) = xs

type family HListFirst a :: * where
    HListFirst 'Nil = ()
    HListFirst ('Cons a _) = a

hListFirst :: HList a -> HListFirst a
hListFirst HNil = ()
hListFirst (HCons a xs) = a

class HListSplit (a::List *) (b::List *) where
    hListSplit :: HList (HListConcat a b) -> (HList a, HList b)

instance HListSplit 'Nil a where
    hListSplit xs = (HNil, xs)

instance HListSplit b c => HListSplit ('Cons a b) c where
    hListSplit (HCons x xs) =
        (HCons x as, bs)
            where (as, bs) = hListSplit xs