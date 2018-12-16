module Cooper
    ( ComposeWith(..)
    , HasTrace(..)
    , Simple
    , Val(Val)
    , ($$)
    , apply
    , lift
    , retrieve
    , retrieve2
    , retrieve3
    , retrieve4
    , unlift
    , store
    ) where

import HList (HList(HCons, HNil), HListConcat, HListSplit, List(Cons, Nil), hListConcat, hListFirst, hListRest, hListSplit)

class HasTrace a where
    type TraceOf a :: *

class TraceList (a :: List *) where
    type TracesOf a :: List *

instance TraceList 'Nil where
    type TracesOf 'Nil = 'Nil

instance HasTrace x => TraceList ('Cons x xs) where
    type TracesOf ('Cons x xs) = ('Cons (TraceOf x) (TracesOf xs))

-- a means of composing values of types a and b (in that order) to obtain a
-- value of type c; c uniqely determined by a and b
class ComposeWith a b c | a b -> c where
    composeWith :: a -> b -> c

-- composition by function application
instance ComposeWith (a -> b) a b where
    composeWith = id
instance ComposeWith a (a -> b) b where
    composeWith = flip id

-- composition by predicate modification
instance ComposeWith (a -> Bool) (a -> Bool) (a -> Bool) where
    composeWith p1 p2 x = p1 x && p2 x

data Val s a where
    Val :: TraceList s => HList s -> (HList (TracesOf s) -> b) -> Val (HList s) ((HList (TracesOf s)) -> b)

-- a simple value with an empty store (and hence no parameters)
type Simple a = Val (HList 'Nil) (HList 'Nil -> a)

lift :: a -> Val (HList 'Nil) (HList 'Nil -> a)
lift v = Val HNil (\_ -> v)

store
    :: HasTrace a
    => Val (HList 'Nil) (HList 'Nil -> a)
    -> Val (HList ('Cons a 'Nil)) (HList ('Cons (TraceOf a) 'Nil) -> TraceOf a)
store (Val store a) =
    Val (HCons (a HNil) HNil) (\(HCons x _) -> x)

-- retrieves and applies the first value in the store
retrieve
    :: (TraceList store, ComposeWith s (p -> t) r)
    => Val (HList ('Cons s store)) (HList ('Cons p (TracesOf store)) -> t)
    -> Val (HList store) (HList (TracesOf store) -> r)
retrieve (Val store v) =
    Val (hListRest store)
        (\ps -> composeWith stored (\x -> (v (HCons x ps))))
    where
        stored = hListFirst store

apply
    :: ( ComposeWith f a r
       , HListSplit store1 store2
       , HListSplit (TracesOf store1) (TracesOf store2)
       , TraceList store1, TraceList store2
       , TraceList (HListConcat store1 store2)
       , HListConcat (TracesOf store1) (TracesOf store2) ~ TracesOf (HListConcat store1 store2)
       )
    => Val (HList store1) (HList (TracesOf store1) -> f)
    -> Val (HList store2) (HList (TracesOf store2) -> a)
    -> Val (HList (HListConcat store1 store2)) (HList (HListConcat (TracesOf store1) (TracesOf store2)) -> r)
apply (Val store1 f1) (Val store2 f2) =
    Val (hListConcat store1 store2)
        (\ps ->
            let
                (p1, p2) = hListSplit ps
            in
                composeWith (f1 p1) (f2 p2)
        )

-- operator synonym for 'apply'
($$)
    :: ( ComposeWith f a r
       , HListSplit store1 store2
       , HListSplit (TracesOf store1) (TracesOf store2)
       , TraceList store1, TraceList store2
       , TraceList (HListConcat store1 store2)
       , HListConcat (TracesOf store1) (TracesOf store2) ~ TracesOf (HListConcat store1 store2)
       )
    => Val (HList store1) (HList (TracesOf store1) -> f)
    -> Val (HList store2) (HList (TracesOf store2) -> a)
    -> Val (HList (HListConcat store1 store2)) (HList (HListConcat (TracesOf store1) (TracesOf store2)) -> r)
($$) = apply

infixr 1 $$

unlift :: Val (HList 'Nil) (HList 'Nil -> a) -> a
unlift (Val _ f) = f HNil

-- retrieves and applies the second value in the store
retrieve2
    :: (HasTrace sfst, TraceList store, ComposeWith s (p -> t) r)
    => Val (HList ('Cons sfst ('Cons s store))) (HList ('Cons (TraceOf sfst) ('Cons p (TracesOf store))) -> t)
    -> Val (HList ('Cons sfst store)) (HList ('Cons (TraceOf sfst) (TracesOf store)) -> r)
retrieve2 (Val store v) =
    Val (HCons (hListFirst store) (hListRest (hListRest store)))
        (\ps -> composeWith stored (\x -> (v (HCons (hListFirst ps) (HCons x (hListRest ps))))))
    where
        stored = hListFirst (hListRest store)

-- retrieves and applies the third value in the store
retrieve3
    :: (HasTrace sfst, HasTrace ssnd, TraceList store, ComposeWith s (p -> t) r)
    => Val (HList ('Cons sfst ('Cons ssnd ('Cons s store)))) (HList ('Cons (TraceOf sfst) ('Cons (TraceOf ssnd) ('Cons p (TracesOf store)))) -> t)
    -> Val (HList ('Cons sfst ('Cons ssnd store))) (HList ('Cons (TraceOf sfst) ('Cons (TraceOf ssnd) (TracesOf store))) -> r)
retrieve3 (Val store v) =
    Val (HCons (hListFirst store) (HCons (hListFirst (hListRest store)) (hListRest  (hListRest (hListRest store)))))
        (\ps -> composeWith stored (\x -> (v (HCons (hListFirst ps) (HCons (hListFirst (hListRest ps)) (HCons x (hListRest (hListRest ps))))))))
    where
        stored = hListFirst (hListRest (hListRest store))

-- retrieves and applies the fourth value in the store
retrieve4
    :: (HasTrace sfst, HasTrace ssnd, HasTrace strd, TraceList store, ComposeWith s (p -> t) r)
    => Val (HList ('Cons sfst ('Cons ssnd ('Cons strd ('Cons s store))))) (HList ('Cons (TraceOf sfst) ('Cons (TraceOf ssnd) ('Cons (TraceOf strd) ('Cons p (TracesOf store))))) -> t)
    -> Val (HList ('Cons sfst ('Cons ssnd ('Cons strd store)))) (HList ('Cons (TraceOf sfst) ('Cons (TraceOf ssnd) ('Cons (TraceOf strd) (TracesOf store)))) -> r)
retrieve4 (Val store v) =
    Val (HCons (hListFirst store) (HCons (hListFirst (hListRest store)) (HCons (hListFirst (hListRest (hListRest store))) (hListRest (hListRest (hListRest (hListRest store)))))))
        (\ps -> composeWith stored (\x -> (v (HCons (hListFirst ps) (HCons (hListFirst (hListRest ps)) (HCons (hListFirst (hListRest (hListRest ps))) (HCons x (hListRest (hListRest (hListRest ps))))))))))
    where
        stored = hListFirst (hListRest (hListRest (hListRest store)))