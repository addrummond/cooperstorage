{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
module Main where

import Cooper (HasTrace(TraceOf), Simple, Val(Val), apply, lift, retrieve, run, store)
import ExampleModel (Denotations(..), denotations, model, withModel)

main = return ()

test =
    let
        Denotations{ boy, smokes, every, some, likes, john } = withModel model denotations
    in
        --run (retrieve (store (apply every boy)))
        run (retrieve (apply (apply likes (store (apply every boy))) john))
        --run (apply (apply some boy) smokes)