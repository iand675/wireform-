{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.EDI.Derive.Instances where

import EDI.Derive

import Test.EDI.Derive.Types

deriveEDI ''NameSegment
deriveEDI ''AckCode
deriveEDI ''ShipmentEvent
