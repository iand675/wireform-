{-# LANGUAGE BangPatterns #-}

module Network.HTTP.Methods where

import Control.DeepSeq (NFData (..))
import Data.Binary
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.String
import Symbolize
import Text.Read


newtype Method = Method {fromMethod :: Symbol}
  deriving newtype (Binary, NFData, Eq, Ord, Hashable)


instance IsString Method where
  fromString = Method . fromString . fmap toUpper


instance Read Method where
  readPrec = do
    str <- readPrec @String
    return $ Method $ intern str


instance Show Method where
  showsPrec _ (Method symbol) =
    let !str = unintern @String symbol
    in shows str


{- | Standard HTTP methods defined in RFC 7231
https://datatracker.ietf.org/doc/html/rfc7231
-}

{- | GET method - Requests a representation of the specified resource.
Should only retrieve data and not modify it.
Defined in RFC 7231 Section 4.3.1
-}
mGet :: Method
mGet = "GET"


{- | POST method - Submits an entity to the specified resource.
Often causes a change in state or side effects on the server.
Defined in RFC 7231 Section 4.3.3
-}
mPost :: Method
mPost = "POST"


{- | PUT method - Replaces all current representations of the target resource
with the request payload.
Defined in RFC 7231 Section 4.3.4
-}
mPut :: Method
mPut = "PUT"


{- | DELETE method - Removes the specified resource.
Defined in RFC 7231 Section 4.3.5
-}
mDelete :: Method
mDelete = "DELETE"


{- | HEAD method - Similar to GET but returns only the headers, not the body.
Useful for checking if a resource exists or has been modified.
Defined in RFC 7231 Section 4.3.2
-}
mHead :: Method
mHead = "HEAD"


{- | OPTIONS method - Describes the communication options for the target resource.
Often used for CORS preflight requests.
Defined in RFC 7231 Section 4.3.7
-}
mOptions :: Method
mOptions = "OPTIONS"


{- | TRACE method - Performs a message loop-back test along the path to the target resource.
Useful for debugging.
Defined in RFC 7231 Section 4.3.8
-}
mTrace :: Method
mTrace = "TRACE"


{- | PATCH method - Applies partial modifications to a resource.
Defined in RFC 5789
-}
mPatch :: Method
mPatch = "PATCH"


{- | CONNECT method - Establishes a tunnel to the server identified by the target resource.
Used for SSL/TLS tunneling.
Defined in RFC 7231 Section 4.3.6
-}
mConnect :: Method
mConnect = "CONNECT"


{- | WebDAV Methods (RFC 4918)
https://datatracker.ietf.org/doc/html/rfc4918
-}

{- | ACL method - Modifies the access control list of a resource.
Defined in RFC 3744
-}
mACL :: Method
mACL = "ACL"


{- | BASELINE-CONTROL method - Used in version control operations.
Part of WebDAV Versioning Extensions
-}
mBaselineControl :: Method
mBaselineControl = "BASELINE-CONTROL"


{- | BIND method - Creates a new binding between the specified resource and the request URI.
Part of WebDAV Bindings
-}
mBind :: Method
mBind = "BIND"


{- | CHECKIN method - Checks in a version-controlled resource.
Part of WebDAV Versioning
-}
mCheckin :: Method
mCheckin = "CHECKIN"


{- | CHECKOUT method - Checks out a version-controlled resource.
Part of WebDAV Versioning
-}
mCheckout :: Method
mCheckout = "CHECKOUT"


{- | COPY method - Creates a duplicate of the source resource at the destination.
Defined in RFC 4918 Section 9.8
-}
mCopy :: Method
mCopy = "COPY"


{- | LABEL method - Modifies the labels on a version-controlled resource.
Part of WebDAV Versioning
-}
mLabel :: Method
mLabel = "LABEL"


{- | LINK method - Establishes one or more relationships between the existing resource
and the resources identified in the request body.
Part of WebDAV Bindings
-}
mLink :: Method
mLink = "LINK"


{- | LOCK method - Creates a lock on the specified resource.
Defined in RFC 4918 Section 9.10
-}
mLock :: Method
mLock = "LOCK"


{- | MERGE method - Merges the changes from a checked-out resource into its version history.
Part of WebDAV Versioning
-}
mMerge :: Method
mMerge = "MERGE"


{- | MKACTIVITY method - Creates a new activity resource.
Part of WebDAV Versioning
-}
mMkActivity :: Method
mMkActivity = "MKACTIVITY"


{- | MKCALENDAR method - Creates a new calendar collection resource.
Defined in RFC 4791
-}
mMkCalendar :: Method
mMkCalendar = "MKCALENDAR"


{- | MKCOL method - Creates a new collection resource.
Defined in RFC 4918 Section 9.3
-}
mMkCol :: Method
mMkCol = "MKCOL"


{- | MKREDIRECTREF method - Creates a redirect reference resource.
Part of WebDAV Redirect Reference Resources
-}
mMkRedirectRef :: Method
mMkRedirectRef = "MKREDIRECTREF"


{- | MKWORKSPACE method - Creates a new workspace resource.
Part of WebDAV Workspaces
-}
mMkWorkspace :: Method
mMkWorkspace = "MKWORKSPACE"


{- | MOVE method - Moves a resource from one URI to another.
Defined in RFC 4918 Section 9.9
-}
mMove :: Method
mMove = "MOVE"


{- | ORDERPATCH method - Modifies the ordering of members in a collection.
Part of WebDAV Ordered Collections
-}
mOrderPatch :: Method
mOrderPatch = "ORDERPATCH"


{- | PROPFIND method - Retrieves properties defined on the resource.
Defined in RFC 4918 Section 9.1
-}
mPropFind :: Method
mPropFind = "PROPFIND"


{- | PROPPATCH method - Sets and/or removes properties defined on the resource.
Defined in RFC 4918 Section 9.2
-}
mPropPatch :: Method
mPropPatch = "PROPPATCH"


{- | REBIND method - Removes a binding to a resource and adds a new binding.
Part of WebDAV Bindings
-}
mRebind :: Method
mRebind = "REBIND"


{- | REPORT method - Performs a report on the resource.
Defined in RFC 3253
-}
mReport :: Method
mReport = "REPORT"


{- | SEARCH method - Performs a search on the resource.
Part of WebDAV Search
-}
mSearch :: Method
mSearch = "SEARCH"


{- | UNBIND method - Removes a binding to a resource.
Part of WebDAV Bindings
-}
mUnbind :: Method
mUnbind = "UNBIND"


{- | UPDATE method - Updates a version-controlled resource.
Part of WebDAV Versioning
-}
mUpdate :: Method
mUpdate = "UPDATE"


{- | UPDATEREDIRECTREF method - Updates a redirect reference resource.
Part of WebDAV Redirect Reference Resources
-}
mUpdateDirectRef :: Method
mUpdateDirectRef = "UPDATEDIRECTREF"


{- | VERSION-CONTROL method - Creates a version-controlled resource.
Part of WebDAV Versioning
-}
mVersionControl :: Method
mVersionControl = "VERSION-CONTROL"


{- | UNCHECKOUT method - Cancels a CHECKOUT, restoring the pre-checkout state.
Defined in RFC 3253 (WebDAV Versioning)
-}
mUncheckout :: Method
mUncheckout = "UNCHECKOUT"


{- | UNLINK method - Removes relationships between resources.
Historic (RFC 2068); paired with LINK.
-}
mUnlink :: Method
mUnlink = "UNLINK"


{- | UNLOCK method - Removes a lock from the specified resource.
Defined in RFC 4918 Section 9.11 (WebDAV)
-}
mUnlock :: Method
mUnlock = "UNLOCK"


{- | PRI method - Reserved by HTTP\/2 for the connection preface; never sent
as a real request method.
Defined in RFC 9113 (formerly RFC 7540) Section 3.4
-}
mPri :: Method
mPri = "PRI"


{- | QUERY method - Safe, idempotent method that carries a request body
describing a query, returning a representation of the result.
Defined in RFC 9XXX (HTTP QUERY Method); registered with IANA.
-}
mQuery :: Method
mQuery = "QUERY"
