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

    -- ** Data Structure Affinity
    -- $datastructureaffinity

    -- *** Convex Data
    -- $convexdata

     module Effusion.Convex

    -- *** Concave Data
    -- $concavedata

    ,module Effusion.Concave

    -- *** Inferencing
    -- $inferencing

    ,module Effusion.Inference

    -- * Data Modeling
    -- $datamodeling

    -- ** Abstract Data Model
    -- $abstractdatamodel

    ,module Effusion.Abstract

    -- ** ISA88/ISA95 \"Recipe\" Data Model
    -- $recipedatamodel

    ,module Effusion.Rx

    -- * Data Analysis
    -- $dataanalysis

    -- ** Catamorphisms
    -- $catamorphisms

    ,module Effusion.Catamorphism

    -- ** Genealogy
    -- $genealogy

    ,module Effusion.Genealogy
) where

import Effusion.Convex
import Effusion.Concave
import Effusion.Inference

import Effusion.Abstract
import Effusion.Rx

import Effusion.Catamorphism
import Effusion.Genealogy

-- $datastructureaffinity
-- Effusion's data integration model is based on the concept of /Data structure affinity/, i.e.
-- different sorts of data are better suited for storage in different data persistence system
-- models. Data that is densenly interconnected is said to be /convex/, while data that is
-- sparsely conected is said to be /concave/. These groups are not strictly defined nor mutually
-- exclusive. Indeed, it's certainly possible (but not necessarily easy or efficient) to perform
-- equivalent computations with /any/ data model. Experimentation and profiling are heartily
-- encouraged. Data residing in a non-trivial and idiomatic RDBMS schema might be convex, while
-- streaming output from an industrial control/monitoring system might be concave.

-- $convexdata
-- The "Effusion.Convex" module provides tools for handling densely interconnected data and is
-- intended to be used in conjuction with a data persistence system supporting the RDF data
-- interchange standard and SPARQL query language, although many components may be useful in any
-- system (namely those residing outside of the "Effusion.Convex.SPARQL" and "Effusion.Convex.RDF"
-- modules). The focus of this module is on the construction, transformation, storage, and querying
-- of graphs and common graph patterns.

-- $concavedata
-- The "Effusion.Concave" module provides tools for handling sparsely interconnected, regular data.
-- Tools to handle data of this nature as JSON and in conjunction with MongoDB are also provided
-- (as these are the tools used by the author), however an RDBMS or even a key/value store may be
-- just as effective in many environments. The focus of this module is on handling relationally
-- simple tables, perhaps mergeable on only one or two record members.

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
