{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
module Main where

import Control.Monad (unless)
import Cooper (HasTrace(TraceOf), Simple, Val(Val), (<|), (|>), apply, lift, retrieve, run, store)
import ExampleModel (Denotations(..), Model(..), denotations, model, withModel)

isTrue = run

isFalse = not . run

test =
    let
        Denotations{ likes, every, boy, john, some, smokes } =
            withModel model denotations
    in
    and
        [ isFalse $
              retrieve (john |> (likes <| (store (every <| boy))))
        , isTrue $
              retrieve (john |> (likes <| (store (some <| boy))))
        ]

main :: IO ()
main =
    unless test $
        error "Unexpected result"