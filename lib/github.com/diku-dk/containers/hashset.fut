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
  val member [n] [f] : ctx -> k -> hashset [n] [f] -> bool

  -- | Check if a key is not member of the hashset
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [n] [f] : ctx -> k -> hashset [n] [f] -> bool

  -- | Given an array keys construct a hashset.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [u] : ctx -> rng -> [u]k -> ?[n][f].(rng, hashset [n] [f])

  -- | Given an array keys construct a hashset. If the given keys
  -- contains duplicates then the function call will never finish.
  -- Inturn it does less work.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val unsafe_from_array [u] : ctx -> rng -> [u]k -> ?[n][f].(rng, hashset [n] [f])

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

  -- | Gets the context of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val context [n] [f] : hashset [n] [f] -> ctx

  -- | Insert new keys into a hashset. If a key already exists in the
  -- hashset, the new value will overwrite the old one.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log (n + u))*
  val insert [n] [f] [u] :
    ctx -> rng -> hashset [n] [f] -> [u]k -> ?[n'][f'].(rng, hashset [n'] [f'])
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

  def unsafe_from_array [u]
                        (ctx: ctx)
                        (r: rng)
                        (keys: [u]k) : ?[n][f].(rng, hashset [n] [f]) =
    hashmap.unsafe_from_array_replicate ctx r keys ()

  def to_array [n] [f] (set: hashset [n] [f]) : []k =
    hashmap.to_array set
    |> map (.0)

  def size [n] [f] (set: hashset [n] [f]) =
    hashmap.size set

  def context [n] [f] (set: hashset [n] [f]) =
    hashmap.context set

  def member [n] [f]
             (ctx: ctx)
             (key: k)
             (set: hashset [n] [f]) : bool =
    hashmap.member ctx key set

  def not_member [n] [f]
                 (ctx: ctx)
                 (key: k)
                 (set: hashset [n] [f]) : bool =
    hashmap.not_member ctx key set

  def insert [n] [f] [u]
             (ctx: ctx)
             (r: rng)
             (set: hashset [n] [f])
             (keys: [u]k) : ?[n'][f'].(rng, hashset [n'] [f']) =
    hashmap.insert ctx r set (zip keys (replicate u ()))
}

module type hashset = {
  type k

  type ctx

  type rng

  type~ hashset

  val member : ctx -> k -> hashset -> bool

  val not_member : ctx -> k -> hashset -> bool

  val from_array [n] : ctx -> rng -> [n]k -> (rng, hashset)

  val unsafe_from_array [n] : ctx -> rng -> [n]k -> (rng, hashset)

  val to_array : hashset -> []k

  val size : hashset -> i64

  val context : hashset -> ctx

  val insert [u] :
    ctx -> rng -> hashset -> [u]k -> (rng, hashset)
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

  def unsafe_from_array [n]
                        (ctx: ctx)
                        (r: rng)
                        (keys: [n]k) : (rng, hashset) =
    hashset.unsafe_from_array ctx r keys

  def to_array (set: hashset) : []k =
    hashset.to_array set

  def size (set: hashset) =
    hashset.size set

  def context (set: hashset) =
    hashset.context set

  def member (ctx: ctx) (key: k) (set: hashset) : bool =
    hashset.member ctx key set

  def not_member (ctx: ctx) (key: k) (set: hashset) : bool =
    hashset.not_member ctx key set

  def insert [n]
             (ctx: ctx)
             (r: rng)
             (set: hashset)
             (keys: [n]k) : (rng, hashset) =
    hashset.insert ctx r set keys
}
