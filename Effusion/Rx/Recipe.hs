{-|
Module         : Effusion.Rx.Recipe
Description    : Framework for ISA88/ISA95 Modeling
Copyright      : Travis Whitaker 2014-2015
License        : All rights reserved.
Maintainer     : twhitak@its.jnj.com
Stability      : Provisional
Portability    : POSIX

Document this.
-}

{-# LANGUAGE TypeFamilies #-}

module Effusion.Rx.Recipe (
    StaticRecipe(..)
   ,StaticRecipeStatus(..)
--   ,ProcessRecipe(..)
   ,GeneralRecipe(..)
   ,SiteRecipe(..)
   ,MasterRecipe(..)
   ,ControlRecipe(..)
) where

import Data.ByteString.Char8 as C

import Effusion.Rx.Step

class StaticRecipe r where
    type Child r :: *
    children     :: r -> [Child r]
    process      :: r -> Process
    name         :: r -> C.ByteString
    status       :: r -> StaticRecipeStatus
    product      :: r -> Product
    author       :: r -> Person
    approver     :: r -> Person
    orgSite      :: r -> OrgSite
    version      :: r -> Double
    lastEditor   :: r -> Person
    description  :: r -> C.ByteString
    lastDate     :: r -> C.ByteString
    material     :: r -> Material

data StaticRecipeStatus = Unknown | Draft | Approved | Review deriving (Show, Eq)

data PlatformRecipe = PlatformRecipe {
    prxChildren    :: [GeneralRecipe]
   ,prxProcess     :: Process
   ,prxName        :: C.ByteString
   ,prxStatus      :: StaticRecipeStatus
   ,prxProduct     :: Product
   ,prxAuthor      :: Person
   ,prxApprover    :: Person
   ,prxOrgSite     :: OrgSite
   ,prxVersion     :: Double
   ,prxLastEditor  :: Person
   ,prxDescription :: C.ByteString
   ,prxLastDate    :: C.ByteString
   ,prxMaterial    :: Material
} deriving (Show, Eq)

instance StaticRecipe PlatformRecipe where
    Child PlatformRecipe = Generalrecipe
    children             = prxChildren
    process              = prxProcess
    rxName                 = prxName
    status               = prxStatus
    product              = prxProduct
    author               = prxAuthor
    approver             = prxApprover
    orgSite              = prxOrgSite
    version              = prxVersion
    lastEditor           = prxLastEditor
    description          = prxDescription
    lastDate             = prxLastDate
    material             = prxMaterial

data GeneralRecipe = GeneralRecipe {
    grxChildren    :: [SiteRecipe]
   ,grxProcess     :: Process
   ,grxName        :: C.ByteString
   ,grxStatus      :: StaticRecipeStatus
   ,grxProduct     :: Product
   ,grxAuthor      :: Person
   ,grxApprover    :: Person
   ,grxOrgSite     :: OrgSite
   ,grxVersion     :: Double
   ,grxLastEditor  :: Person
   ,grxDescription :: C.ByteString
   ,grxLastDate    :: C.ByteString
   ,grxMaterial    :: Material
} deriving (Show, Eq)

instance StaticRecipe GeneralRecipe where
    Child GeneralRecipe = SiteRecipe
    children            = grxChildren
    process             = grxProcess
    rxName                = grxName
    status              = grxStatus
    product             = grxProduct
    author              = grxAuthor
    approver            = grxApprover
    orgSite             = grxOrgSite
    version             = grxVersion
    lastEditor          = grxLastEditor
    description         = grxDescription
    lastDate            = grxLastDate
    material            = grxMaterial

data SiteRecipe = SiteRecipe {
    srxChildren    :: [MasterRecipe]
   ,srxProcess     :: Process
   ,srxName        :: C.ByteString
   ,srxStatus      :: StaticRecipeStatus
   ,srxProduct     :: Product
   ,srxAuthor      :: Person
   ,srxApprover    :: Person
   ,srxOrgSite     :: OrgSite
   ,srxVersion     :: Double
   ,srxLastEditor  :: Person
   ,srxDescription :: C.ByteString
   ,srxLastDate    :: C.ByteString
   ,srxMaterial    :: Material
} deriving (Show, Eq)

instance StaticRecipe SiteRecipe where
    Child SiteRecipe = MasterRecipe
    children         = srxChildren
    process          = srxProcess
    rxName             = srxName
    status           = srxStatus
    product          = srxProduct
    author           = srxAuthor
    approver         = srxApprover
    orgSite          = srxOrgSite
    version          = srxVersion
    lastEditor       = srxLastEditor
    description      = srxDescription
    lastDate         = srxLastDate
    material         = srxMaterial

data MasterRecipe = MasterRecipe {
    mrxChildren    :: [ControlRecipe]
   ,mrxProcess     :: Process
   ,mrxName        :: C.ByteString
   ,mrxStatus      :: StaticRecipeStatus
   ,mrxProduct     :: Product
   ,mrxAuthor      :: Person
   ,mrxApprover    :: Person
   ,mrxOrgSite     :: OrgSite
   ,mrxVersion     :: Double
   ,mrxLastEditor  :: Person
   ,mrxDescription :: C.ByteString
   ,mrxLastDate    :: C.ByteString
   ,mrxMaterial    :: Material
} deriving (Show, Eq)

instance StaticRecipe MasterRecipe where
    Child MasterRecipe = ControlRecipe
    children           = mrxChildren
    process            = mrxProcess
    rxName               = mrxName
    status             = mrxStatus
    product            = mrxProduct
    author             = mrxAuthor
    approver           = mrxApprover
    orgSite            = mrxOrgSite
    version            = mrxVersion
    lastEditor         = mrxLastEditor
    description        = mrxDescription
    lastDate           = mrxLastDate
    material           = mrxMaterial

data ControlRecipe = ControlRecipe {
    crxBatchProcess :: BatchProcess
   ,crxBatchNumber  :: C.ByteString
   ,crxBatchSize    :: Double
   ,crxLastDate     :: C.ByteString
} deriving (Show, Eq)
