{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
module ExampleModel
    ( Denotations(..)
    , E
    , Model(..)
    , denotations
    , model
    , withModel
    ) where

import Cooper (HasTrace(TraceOf), Simple, lift)

data E = John | Tom | Bill | Jane deriving Eq

instance HasTrace ((E -> Bool) -> Bool) where
    type TraceOf ((E -> Bool) -> Bool) = E

data Model = Model
    { individuals :: [E]
    , boys        :: [E]
    , girls       :: [E]
    , smokers     :: [E]
    , dancers     :: [E]
    , likings     :: [(E, E)]
    , detestings  :: [(E, E)]
    }

model :: Model
model = Model
    { individuals = [John, Tom, Bill, Jane]
    , boys        = [John, Tom, Bill]
    , girls       = [Jane]
    , smokers     = [John, Jane]
    , dancers     = [John, Bill]
    , likings     = [(John, Tom), (Bill, Jane)]
    , detestings  = [(Jane, Tom)]
    }

-- Id and Domain can be used to fake a type-level (* -> *) identity function.
-- This is a technique used in the type-functions package.
data Id a = Id a
type family Domain a where
    Domain (Id a) = a
    Domain a = a

data Denotations f = Denotations
    { boy     :: Domain (f (Simple (E -> Bool)))
    , girl    :: Domain (f (Simple (E -> Bool)))
    , smokes  :: Domain (f (Simple (E -> Bool)))
    , dances  :: Domain (f (Simple (E -> Bool)))
    , likes   :: Domain (f (Simple (E -> E -> Bool)))
    , detests :: Domain (f (Simple (E -> E -> Bool)))
    , every   :: Domain (f (Simple ((E -> Bool) -> ((E -> Bool) -> Bool))))
    , some    :: Domain (f (Simple ((E -> Bool) -> ((E -> Bool) -> Bool))))
    , john    :: Domain (f (Simple E))
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
