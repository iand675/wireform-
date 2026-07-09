{- | Lattice: a cache-native graph query protocol.

Umbrella entry module — the default surface. @import Lattice@ brings in:

* the query \/ schema value vocabulary ("Lattice.Types");
* the typed row + loader-authoring surface ("Lattice.Typed") — HKD entity
  records, the 'Lattice.Typed.LatticeValue' \/ 'Lattice.Typed.LatticeEntity'
  codecs, and the @found@ \/ @absent@ \/ @loaders@ loader combinators;
* the IDL-to-Haskell codegen splice ("Lattice.TH": @latticeTypes@).

Reach for the granular modules ("Lattice.Backend", "Lattice.Server", …)
only for the backend contract and origin machinery around this core.
-}
module Lattice (
  module Lattice.Types,
  module Lattice.Typed,
  module Lattice.TH,
) where

import Lattice.TH
import Lattice.Typed
import Lattice.Types
