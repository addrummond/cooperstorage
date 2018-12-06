{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
module Main where

import Control.Monad (unless)
import Cooper (HasTrace(TraceOf), Simple, Val(Val), apply, lift, retrieve, run, store)
import ExampleModel (Denotations(..), Model(..), denotations, model, withModel)

test =
    let
        Denotations{ likes, every, boy, john, some, smokes } =
            withModel model denotations
    in
    not (run (retrieve (apply (apply likes (store (apply every boy))) john))) &&
    run (retrieve (apply (apply likes (store (apply some boy))) john))

main :: IO ()
main =
    unless test $
        error "Unexpected result"