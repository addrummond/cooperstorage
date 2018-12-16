module ExampleModel
    ( Denots(..)
    , E
    , Model(..)
    , denotations
    , evalExample
    , composeExample
    , model
    , withModel
    ) where

import Cooper (HasTrace(TraceOf), Simple, lift, unlift)

data E = John | Tom | Bill | Jane | Mary | Carol deriving Eq

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
    { individuals = [John, Tom, Bill, Jane, Mary, Carol]
    , boys        = [John, Tom, Bill]
    , girls       = [Jane, Mary, Carol]
    , smokers     = [John, Jane]
    , dancers     = [John, Bill, Tom, Jane]
    , likings     = [(John, Tom), (Bill, Jane), (Bill, Mary), (Bill, Carol), (Carol, Mary)]
    , detestings  = [(Jane, Tom)]
    }

-- Id and Domain can be used to fake a type-level (* -> *) identity function.
-- This is a technique used in the type-functions package.
data Id a = Id a
type family Domain a where
    Domain (Id a) = a
    Domain a = a

data Denots f = Denots
    { boy     :: Domain (f (Simple (E -> Bool)))
    , girl    :: Domain (f (Simple (E -> Bool)))
    , smokes  :: Domain (f (Simple (E -> Bool)))
    , dances  :: Domain (f (Simple (E -> Bool)))
    , likes   :: Domain (f (Simple (E -> E -> Bool)))
    , detests :: Domain (f (Simple (E -> E -> Bool)))
    , every   :: Domain (f (Simple ((E -> Bool) -> ((E -> Bool) -> Bool))))
    , some    :: Domain (f (Simple ((E -> Bool) -> ((E -> Bool) -> Bool))))
    , john    :: Domain (f (Simple E))
    , tom     :: Domain (f (Simple E))
    , bill    :: Domain (f (Simple E))
    , jane    :: Domain (f (Simple E))
    , mary    :: Domain (f (Simple E))
    , carol   :: Domain (f (Simple E))
    }

denotations :: Denots ((->) Model)
denotations = Denots
    { boy     = \Model{ boys } -> lift $ \x -> x `elem` boys
    , girl    = \Model{ girls } -> lift $ \x -> x `elem` girls
    , smokes  = \Model{ smokers } -> lift $ \x -> x `elem` smokers
    , dances  = \Model{ dancers } -> lift $ \x -> x `elem` dancers
    , likes   = \Model{ likings } -> lift $ \x y -> (y,x) `elem` likings
    , detests = \Model{ detestings } -> lift $ \x y -> (y,x) `elem` detestings
    , every   = \Model{ individuals } -> lift $ \u v -> all (\x -> not (u x) || v x ) individuals
    , some    = \Model{ individuals } -> lift $ \u v -> any (\x -> u x && v x) individuals
    , john    = \_ -> lift John
    , tom     = \_ -> lift John
    , bill    = \_ -> lift John
    , jane    = \_ -> lift John
    , mary    = \_ -> lift John
    , carol   = \_ -> lift John
    }

withModel :: Model -> Denots ((->) Model) -> Denots Id
withModel m Denots{ boy, girl, smokes, dances, likes, detests, every, some, john, tom, bill, jane, mary, carol } = Denots
    { boy = boy m   
    , girl = girl m
    , smokes = smokes m
    , dances = dances m
    , likes = likes m
    , detests = detests m
    , every = every m
    , some = some m
    , john = john m
    , tom = tom m
    , bill = bill m
    , jane = jane m
    , mary = mary m
    , carol = carol m
    }

-- Utility function that's useful in ghci.
evalExample f = unlift (f ds)
    where
        ds = withModel model denotations

composeExample f = f ds
    where
        ds = withModel model denotations