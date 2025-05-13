-- | Static Hashset module
--
-- | This is an implementation of a static hash table using [two level
-- hashing](https://en.wikipedia.org/wiki/Double_hashing). The modules time
-- complexities assumes only unique keys but the modules does work with
-- duplicate keys.

import "../cpprandom/random"
import "hashset_unlifted"
import "key"

module type hashset = {
  -- | The key type.
  type k

  -- | The context type.
  type ctx

  -- | The random number generator.
  type rng

  -- | The hashset type.
  type~ hashset

  -- | Check if a key is member of the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member : k -> hashset -> bool

  -- | Check if a key is not member of the hashset
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member : k -> hashset -> bool

  -- | Given a random number generator, equality, hash function, and
  -- an array keys construct a hashset. Assumes unique keys but works
  -- with duplicates but too many may lead to unintended behavior
  -- or excessive memory allocations.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [n] : ctx -> rng -> [n]k -> (rng, hashset)

  -- | Convert hashset to an array of keys.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val to_array : hashset -> []k

  -- | The number of elements in the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size : hashset -> i64
}

module hashset (K: key) (E: rng_engine with int.t = K.i)
  : hashset
    with rng = E.rng
    with k = K.k
    with ctx = K.ctx = {
  module hashset = hashset K E
  type rng = hashset.rng
  type k = hashset.k
  type ctx = hashset.ctx

  type~ hashset = ?[n][w][f].hashset.hashset [n] [w] [f]

  def from_array [n]
                 (ctx: ctx)
                 (r: rng)
                 (keys: [n]k) : (rng, hashset) =
    hashset.from_array ctx r keys

  def to_array (set: hashset) : []k =
    hashset.to_array set

  def size (set: hashset) =
    hashset.size set

  def member (key: k) (set: hashset) : bool =
    hashset.member key set

  def not_member (key: k) (set: hashset) : bool =
    hashset.not_member key set
}
