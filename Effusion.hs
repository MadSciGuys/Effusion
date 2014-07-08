{-|
Module      : Effusion
Description : Framework for Semantic Industrial Data Management
Copyright   : Travis Whitaker 2014
License     : All rights reserved.
Maintainer  : twhitak@its.jnj.com
Stability   : Provisional
Portability : POSIX

Effusion is a generic Haskell toolkit for implementing semantic industrial data management systems.
The individual library modules are designed to be used in concert, although they may be useful on
their own. Functionality may be placed into three (nonmutually exclusive) categories,
/Data Integration/, /Data Modeling/, and /Data Analysis/.
-}

module Effusion (

    -- * Data Integration

    -- ** Transformation
    -- $transformation

     module Effusion.Data

    -- ** Inferencing
    -- $inferencing

    ,module Effusion.Inference

    -- * Data Modeling
    -- $datamodeling

    -- ** Abstract Data Model
    -- $abstractdatamodel

    ,module Effusion.Abstract

    -- ** ISA88/ISA95 \"Recipe\" Data Model
    -- $recipedatamodel

--    ,module Effusion.Rx

    -- * Data Analysis
    -- $dataanalysis

    -- ** Catamorphisms
    -- $catamorphisms

    ,module Effusion.Catamorphism

    -- ** Genealogy
    -- $genealogy

    ,module Effusion.Genealogy

    -- ** Numerics
    -- $numerics

    ,module Effusion.Numerics
) where

import Effusion.Data
import Effusion.Inference

import Effusion.Abstract
--import Effusion.Rx

import Effusion.Catamorphism
import Effusion.Genealogy
import Effusion.Numerics

-- $transformation
-- Something about transformation.

-- $inferencing
-- The "Effusion.Inference" module provides generic tools for data integration, like merging data
-- sets on shared identifiers, record linking, string analysis, and fuzzy string matching. Common
-- data interchange structures like tables, trees, and graphs are the focus.

-- $datamodeling
-- Something about data modeling.

-- $abstractdatamodel
-- Something about abstract data model.

-- $recipedatamodel
-- Something about recipe data model.

-- $dataanalysis
-- Something about data analysis.

-- $catamorphisms
-- Something about catamorphisms.

-- $genealogy
-- Something about genealogy.

-- $numerics
-- Something about numerics.
