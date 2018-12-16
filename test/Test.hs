{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}
module Main where

import Control.Monad (unless)
import Cooper (HasTrace(TraceOf), Simple, Val, ($$), apply, lift, retrieve, retrieve2, retrieve3, retrieve4, unlift, store)
import ExampleModel (Denotations(..), Model(..), denotations, model, withModel)

isTrue = unlift

isFalse = not . unlift

test =
    let
        Denotations{ likes, every, boy, girl, john, some, smokes } =
            withModel model denotations
    in
    and
        [ isFalse $
            retrieve (john $$ (likes $$ (store (every $$ boy))))
        , isTrue $
            retrieve (john $$ (likes $$ (store (some $$ boy))))
        , isTrue $
            retrieve ((some $$ boy) $$ (likes $$ (store (some $$ girl))))
        , isTrue $
            -- there is a boy x such that for every girl y, x likes y
            retrieve ((some $$ boy) $$ (likes $$ (store (every $$ girl))))
        , isTrue $
            -- for every girl x, there is a boy y such that y likes x
            retrieve (retrieve ((store (some $$ boy)) $$ (likes $$ (store (every $$ girl)))))
        , isTrue $
            -- there is a boy x such that for every girl y, x likes y
            retrieve (retrieve2 ((store (some $$ boy)) $$ (likes $$ (store (every $$ girl)))))
        , isFalse $
            -- there is a girl x such that for every boy y, x likes y
            retrieve ((some $$ girl) $$ (likes $$ (store (every $$ boy))))
        ]

main :: IO ()
main =
    unless test $
        error "Unexpected result"