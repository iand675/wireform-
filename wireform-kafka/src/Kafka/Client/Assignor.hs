{- |
Module      : Kafka.Client.Assignor
Description : Custom consumer-group assignors

A 'CustomAssignor' replaces the built-in range / round-robin / sticky
strategies: the group leader hands it every member's subscription
(topics, user data, partitions it owned) and the partition count of
every subscribed topic, and it returns each member's partitions plus
an opaque user-data blob delivered to that member with its assignment
(see 'Kafka.Client.Consumer.setAssignmentHook'). This is the
mechanism behind application-level assignors such as Kafka Streams'
task assignor.

Every member of a group must advertise the same assignor name: the
coordinator only picks a protocol every member supports.
-}
module Kafka.Client.Assignor (
  CustomAssignor (..),
  MemberSubscription (..),
  MemberAssignment (..),
) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Text (Text)


-- | One member's JoinGroup subscription, as the leader sees it.
data MemberSubscription = MemberSubscription
  { msMemberId :: !Text
  , msTopics :: ![Text]
  , msUserData :: !ByteString
  , msOwned :: ![(Text, [Int32])]
  -- ^ Partitions it owned in the previous generation
  }
  deriving stock (Eq, Show)


data MemberAssignment = MemberAssignment
  { maPartitions :: ![(Text, [Int32])]
  , maUserData :: !ByteString
  }
  deriving stock (Eq, Show)


data CustomAssignor = CustomAssignor
  { caName :: !Text
  -- ^ The protocol name advertised in JoinGroup
  , caAssign :: !(Map Text Int32 -> [MemberSubscription] -> IO (Either String (Map Text MemberAssignment)))
  {- ^ Partition count per subscribed topic, every member's subscription;
  a member left out of the result gets nothing
  -}
  }


instance Show CustomAssignor where
  showsPrec d ca = showParen (d > 10) (showString "CustomAssignor " . showsPrec 11 (caName ca))


-- | By name: the coordinator matches assignors by name too.
instance Eq CustomAssignor where
  a == b = caName a == caName b
