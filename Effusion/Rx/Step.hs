{-|
Module         : Effusion.Rx.Step
Description    : Framework for ISA88/ISA95 Modeling
Copyright      : Travis Whitaker 2014
License        : All rights reserved.
Maintainer     : twhitak@its.jnj.com
Stability      : Provisional
Portability    : POSIX

Document this.
-}

{-# LANGUAGE TypeFamilies #-}

module Effusion.Rx.Step (
    StaticStep(..)
   ,Process(..)
   ,Stage(..)
   ,Operation(..)
   ,Action(..)
) where

import Data.ByteString.Char8 as C

class StaticStep s where
    type Child s :: *
    children     :: s -> [Child r]

data StepArgAction = Input | Output | IntputOutput deriving (Show, Eq)

data StepArg = StepIngredient {
        action     :: StepArgAction
       ,ingredient :: Ingredient
       ,amount     :: Double
    }
             | StepInstrument {
        action         :: StepArgAction
       ,instrument     :: Instrument
       ,instrumentRole :: InstrumentRole
    }
             | StepParam {
        action         :: StepArgAction
       ,parameter      :: Parameter
       ,instrumentType :: InstrumentType
    }
             | StepPerson {
    }
    deriving (Show, Eq)

data Process = Process {
    pName :: C.ByteString
   ,pAuthor :: Person
   ,pLastPerson :: Person
   ,pDescription :: C.ByteString
   ,pLastDate :: C.ByteString
   ,pStepArgs :: [StepArg]
} deriving (Show, Eq)
