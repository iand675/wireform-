{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.EDI.Derive.Types where

import Data.Coerce (coerce)
import Data.Text (Text)
import GHC.Generics (Generic)

import EDI.Class
import Wireform.Derive

newtype CustomerId = CustomerId { unCustomerId :: Text }
  deriving stock (Show, Eq, Generic)

newtype CustomerIdWire = CustomerIdWire { unCustomerIdWire :: Text }
  deriving stock (Show, Eq, Generic)

instance ToEDIField CustomerIdWire where
  toEDIElement = toEDIElement . unCustomerIdWire

instance FromEDIField CustomerIdWire where
  fromEDIElement elemValue = CustomerIdWire <$> fromEDIElement elemValue

data NameSegment = NameSegment
  { nameEntityCode :: !Text
  , nameValue :: !Text
  , namePrivate :: !Text
  , nameCustomerId :: !CustomerId
  , nameOptionalCode :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

defaultPrivate :: Text
defaultPrivate = "internal"

{-# ANN NameSegment (forBackend backendEDI (rename "N1")) #-}
{-# ANN namePrivate skip #-}
{-# ANN namePrivate (defaults 'defaultPrivate) #-}
{-# ANN nameCustomerId (coerced ''CustomerIdWire) #-}
{-# ANN nameOptionalCode optional #-}

data AckCode
  = Accepted
  | Rejected
  deriving stock (Show, Eq, Generic)

{-# ANN Accepted (forBackend backendEDI (rename "A")) #-}
{-# ANN Rejected (forBackend backendEDI (rename "R")) #-}

data ShipmentEvent
  = ShipmentCreated !Text
  | ShipmentClosed
  | ShipmentMoved !Text !Int
  deriving stock (Show, Eq, Generic)

{-# ANN ShipmentCreated (forBackend backendEDI (rename "SC")) #-}
{-# ANN ShipmentClosed (forBackend backendEDI (rename "SX")) #-}
{-# ANN ShipmentMoved (forBackend backendEDI (rename "SM")) #-}

coercedCustomer :: Text -> CustomerId
coercedCustomer = coerce . CustomerIdWire
