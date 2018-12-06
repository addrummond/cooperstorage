{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
module Main where

import HList (HList(HCons, HNil), HListConcat, HListSplit, List(Cons, Nil), hListConcat, hListFirst, hListRest, hListSplit)

main = return ()

class HasTrace a where
    type TraceOf a :: *

data Val s a where
    Val :: HList s -> (HList a -> b) -> Val (HList s) ((HList a) -> b)

type Simple a = Val (HList 'Nil) (HList 'Nil -> a)
 
lift :: a -> Val (HList 'Nil) (HList 'Nil -> a)
lift v = Val HNil (\_ -> v)

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

run :: Val (HList 'Nil) (HList 'Nil -> a) -> a
run (Val _ f) = f HNil

data Individual = John | Tom | Bill | Jane deriving Eq

instance HasTrace ((Individual -> Bool) -> Bool) where
    type TraceOf ((Individual -> Bool) -> Bool) = Individual

data Model = Model
    { individuals :: [Individual]
    , boys        :: [Individual]
    , girls       :: [Individual]
    , smokers     :: [Individual]
    , dancers     :: [Individual]
    , likings     :: [(Individual, Individual)]
    , detestings  :: [(Individual, Individual)]
    }

-- Id and Domain can be used to fake a type-level (* -> *) identity function.
-- This is a technique used in the type-functions package.
data Id a = Id a
type family Domain a where
    Domain (Id a) = a
    Domain a = a

data Denotations f = Denotations
    { boy     :: Domain (f (Simple (Individual -> Bool)))
    , girl    :: Domain (f (Simple (Individual -> Bool)))
    , smokes  :: Domain (f (Simple (Individual -> Bool)))
    , dances  :: Domain (f (Simple (Individual -> Bool)))
    , likes   :: Domain (f (Simple (Individual -> Individual -> Bool)))
    , detests :: Domain (f (Simple (Individual -> Individual -> Bool)))
    , every   :: Domain (f (Simple ((Individual -> Bool) -> ((Individual -> Bool) -> Bool))))
    , some    :: Domain (f (Simple ((Individual -> Bool) -> ((Individual -> Bool) -> Bool))))
    , john    :: Domain (f (Simple Individual))
    }

denotations :: Denotations ((->) Model)
denotations = Denotations
    { boy     = \Model{ boys } -> lift $ \x -> x `elem` boys
    , girl    = \Model{ girls } -> lift $ \x -> x `elem` girls
    , smokes  = \Model{ smokers } -> lift $ \x -> x `elem` smokers
    , dances  = \Model{ dancers } -> lift $ \x -> x `elem` dancers
    , likes   = \Model{ likings } -> lift $ \x y -> (y,x) `elem` likings
    , detests = \Model{ detestings } -> lift $ \x y -> (y,x) `elem` detestings
    , every   = \Model{ individuals } -> lift $ \u v -> all (\x -> not (u x) || v x ) individuals
    , some    = \Model{ individuals } -> lift $ \u v -> any (\x -> u x && v x) individuals
    , john    = \_ -> lift John
    }

withModel :: Model -> Denotations ((->) Model) -> Denotations Id
withModel m Denotations{ boy, girl, smokes, dances, likes, detests, every, some, john } = Denotations
    { boy = boy m   
    , girl = girl m
    , smokes = smokes m
    , dances = dances m
    , likes = likes m
    , detests = detests m
    , every = every m
    , some = some m
    , john = john m
    }

exampleModel :: Model
exampleModel = Model
    { individuals = [John, Tom, Bill, Jane]
    , boys        = [John, Tom, Bill]
    , girls       = [Jane]
    , smokers     = [John, Jane]
    , dancers     = [John, Bill]
    , likings     = [(John, Tom), (Bill, Jane)]
    , detestings  = [(Jane, Tom)]
    }

test =
    let
        Denotations{ boy, smokes, every, some, likes, john } = withModel exampleModel denotations
    in
        --run (retrieve (store (apply every boy)))
        run (retrieve (apply (apply likes (store (apply every boy))) john))
        --run (apply (apply some boy) smokes)