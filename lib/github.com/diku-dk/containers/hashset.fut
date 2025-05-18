-- | Static hashsets with an unlifted type.
--
-- This is an implementation of a static hash table using [two level
-- hashing](https://en.wikipedia.org/wiki/Double_hashing). The modules time
-- complexities assumes only unique keys but the modules does work with
-- duplicate keys.

import "../cpprandom/random"
import "hashmap"
import "hashkey"

module type hashset_unlifted = {
  -- | The key type.
  type key

  -- | The context type.
  type ctx

  -- | The hashset type.
  type hashset [n] [f]

  -- | Check if a key is member of the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [n] [f] : ctx -> key -> hashset [n] [f] -> bool

  -- | Check if a key is not member of the hashset
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [n] [f] : ctx -> key -> hashset [n] [f] -> bool

  -- | Given an array keys construct a hashset.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [u] : ctx -> [u]key -> ?[n][f].hashset [n] [f]

  -- | Given an array keys construct a hashset. If the given keys
  -- contains duplicates then the function call will never finish.
  -- Inturn it does less work.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val unsafe_from_array [u] : ctx -> [u]key -> ?[n][f].hashset [n] [f]

  -- | Convert hashset to an array of keys.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val to_array [n] [f] : hashset [n] [f] -> []key

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
    ctx -> hashset [n] [f] -> [u]key -> ?[n'][f'].hashset [n'] [f']
}

module mk_hashset_unlifted (K: hashkey) (E: rng_engine with int.t = u64)
  : hashset_unlifted
    with key = K.key
    with ctx = K.ctx = {
  module hashmap = mk_two_level_hashmap K E
  type key = hashmap.key
  type ctx = hashmap.ctx

  type hashset [n] [f] =
    hashmap.map ctx [n] [f] ()

  def from_array [u]
                 (ctx: ctx)
                 (keys: [u]key) : ?[n][f].hashset [n] [f] =
    hashmap.from_array_rep ctx keys ()

  def unsafe_from_array [u]
                        (ctx: ctx)
                        (keys: [u]key) : ?[n][f].hashset [n] [f] =
    hashmap.unsafe_from_array_rep ctx keys ()

  def to_array [n] [f] (set: hashset [n] [f]) : []key =
    hashmap.to_array set
    |> map (.0)

  def size [n] [f] (set: hashset [n] [f]) =
    hashmap.size set

  def context [n] [f] (set: hashset [n] [f]) =
    hashmap.context set

  def member [n] [f]
             (ctx: ctx)
             (key: key)
             (set: hashset [n] [f]) : bool =
    hashmap.member ctx key set

  def not_member [n] [f]
                 (ctx: ctx)
                 (key: key)
                 (set: hashset [n] [f]) : bool =
    hashmap.not_member ctx key set

  def insert [n] [f] [u]
             (ctx: ctx)
             (set: hashset [n] [f])
             (keys: [u]key) : ?[n'][f'].hashset [n'] [f'] =
    hashmap.insert ctx set (zip keys (replicate u ()))
}

module type hashset = {
  type key

  type ctx

  type~ hashset

  val member : ctx -> key -> hashset -> bool

  val not_member : ctx -> key -> hashset -> bool

  val from_array [n] : ctx -> [n]key -> hashset

  val unsafe_from_array [n] : ctx -> [n]key -> hashset

  val to_array : hashset -> []key

  val size : hashset -> i64

  val context : hashset -> ctx

  val insert [u] :
    ctx -> hashset -> [u]key -> hashset
}

module mk_hashset (K: hashkey) (E: rng_engine with int.t = u64)
  : hashset
    with key = K.key
    with ctx = K.ctx = {
  module hashset = mk_hashset_unlifted K E
  type key = hashset.key
  type ctx = hashset.ctx

  type~ hashset = ?[n][f].hashset.hashset [n] [f]

  def from_array [n]
                 (ctx: ctx)
                 (keys: [n]key) : hashset =
    hashset.from_array ctx keys

  def unsafe_from_array [n]
                        (ctx: ctx)
                        (keys: [n]key) : hashset =
    hashset.unsafe_from_array ctx keys

  def to_array (set: hashset) : []key =
    hashset.to_array set

  def size (set: hashset) =
    hashset.size set

  def context (set: hashset) =
    hashset.context set

  def member (ctx: ctx) (key: key) (set: hashset) : bool =
    hashset.member ctx key set

  def not_member (ctx: ctx) (key: key) (set: hashset) : bool =
    hashset.not_member ctx key set

  def insert [n]
             (ctx: ctx)
             (set: hashset)
             (keys: [n]key) : hashset =
    hashset.insert ctx set keys
}
