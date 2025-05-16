-- | Static hashsets with an unlifted type.
--
-- This is an implementation of a static hash table using [two level
-- hashing](https://en.wikipedia.org/wiki/Double_hashing). The modules time
-- complexities assumes only unique keys but the modules does work with
-- duplicate keys.

import "../cpprandom/random"
import "hashmap"
import "key"

module type hashset_unlifted = {
  -- | The key type.
  type k

  -- | The context type.
  type ctx

  -- | The random number generator.
  type rng

  -- | The hashset type.
  type hashset [n] [f]

  -- | Check if a key is member of the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [n] [f] : k -> hashset [n] [f] -> bool

  -- | Check if a key is not member of the hashset
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [n] [f] : k -> hashset [n] [f] -> bool

  -- | Given an array keys construct a hashset.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [u] : ctx -> rng -> [u]k -> ?[n][f].(rng, hashset [n] [f])

  -- | Convert hashset to an array of keys.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val to_array [n] [f] : hashset [n] [f] -> []k

  -- | The number of elements in the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size [n] [f] : hashset [n] [f] -> i64
}

module mk_hashset_unlifted (K: key) (E: rng_engine with int.t = u64)
  : hashset_unlifted
    with rng = E.rng
    with k = K.k
    with ctx = K.ctx = {
  module hashmap = mk_hashmap_unlifted K E
  type rng = hashmap.rng
  type k = hashmap.k
  type ctx = hashmap.ctx

  type hashset [n] [f] =
    hashmap.hashmap ctx [n] [f] ()

  def from_array [u]
                 (ctx: ctx)
                 (r: rng)
                 (keys: [u]k) : ?[n][f].(rng, hashset [n] [f]) =
    hashmap.from_array_replicate ctx r keys ()

  def to_array [n] [f] (set: hashset [n] [f]) : []k =
    hashmap.to_array set
    |> map (.0)

  def size [n] [f] (set: hashset [n] [f]) =
    hashmap.size set

  def member [n] [f]
             (key: k)
             (set: hashset [n] [f]) : bool =
    hashmap.member key set

  def not_member [n] [f]
                 (key: k)
                 (set: hashset [n] [f]) : bool =
    hashmap.not_member key set
}

module type hashset = {
  type k

  type ctx

  type rng

  type~ hashset

  val member : k -> hashset -> bool

  val not_member : k -> hashset -> bool

  val from_array [n] : ctx -> rng -> [n]k -> (rng, hashset)

  val to_array : hashset -> []k

  val size : hashset -> i64
}

module mk_hashset (K: key) (E: rng_engine with int.t = u64)
  : hashset
    with rng = E.rng
    with k = K.k
    with ctx = K.ctx = {
  module hashset = mk_hashset_unlifted K E
  type rng = hashset.rng
  type k = hashset.k
  type ctx = hashset.ctx

  type~ hashset = ?[n][f].hashset.hashset [n] [f]

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
