-- | Static Hashmap module
--
-- | This is an implementation of a static hash table using two level
-- hasing https://en.wikipedia.org/wiki/Double_hashing. The modules
-- time complexities assumes only unique keys but the modules does
-- work with duplicate keys.

import "key"
import "hashmap_unlifted"
import "../cpprandom/random"

module type hashmap = {
  -- | The key type.
  type k

  -- | The random number generator.
  type rng

  -- | The hashmap definitionen.
  type~ hashmap 'v

  -- | Check if a key is member of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member 'v : k -> hashmap v -> bool

  -- | Check if a key is not member of the hashmap
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member 'v : k -> hashmap v -> bool

  -- | Given a random number generator, equality, hash function, and
  -- a key-value array construct a hashmap. Assumes unique keys but
  -- works with duplicates but too many may lead to unintended
  -- behavior or excessive memory allocations. Keys with the smallest
  -- hash will be priotized.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [n] 'v : rng -> [n](k, v) -> (rng, hashmap v)

  -- | Create hashmap with default value.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_fill [n] 'v : rng -> [n]k -> v -> (rng, hashmap v)

  -- | Create hashmap where duplicates are reduced with an commutative
  -- and associative operation.
  --
  -- **Expected Work:** *O(s + n ✕ W(op))*
  --
  -- **Expected Span:** *O(log n)* (Assuming best case for hist)
  val from_array_hist [n] 'v :
    rng -> [n](k, v) -> (v -> v -> v) -> v -> (rng, hashmap v)

  -- | Compute a histogram using the given key value pairs.
  --
  -- Same asymptotics as the hist SOAC.
  val hashmap_hist [n] 'v : hashmap v -> [n](k, v) -> (v -> v -> v) -> v -> hashmap v

  -- | Map a function over the hashmap values.
  --
  -- **Work:** *O(s ✕ W(g))*
  --
  -- **Span:** *O(s(g))*
  val hashmap_map 'v 't : (g: v -> t) -> hashmap v -> hashmap t

  -- | Convert hashmap to an array of key-value pairs.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val to_array 'v : hashmap v -> [](k, v)

  -- | Updates the value of the hash map using the key with the
  -- smallest index.
  --
  -- **Work:** *O(s + u)*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val update [u] 'v :
    [u](k, v)
    -> hashmap v
    -> hashmap v

  -- | The number of elements in the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size 'k 'v : hashmap v -> i64
}

module hashmap (K: key) (E: rng_engine with int.t = K.i)
  : hashmap
    with rng = E.rng
    with k = K.k = {
  module hashmap = hashmap K E
  type rng = hashmap.rng
  type k = hashmap.k

  type~ hashmap 'v =
    ?[n][w][f][s].hashmap.hashmap [n] [w] [f] [s] v

  def from_array [n] 'v (r: rng) (key_values: [n](k, v)) : (rng, hashmap v) =
    hashmap.from_array r key_values

  def from_array_fill [n] 'v (r: rng) (keys: [n]k) (ne: v) : (rng, hashmap v) =
    hashmap.from_array_fill r keys ne

  def hashmap_hist [n] 'v (hmap: hashmap v) (key_values: [n](k, v)) (op: v -> v -> v) (ne: v) =
    hashmap.hashmap_hist hmap key_values op ne

  def from_array_hist [n] 'v (r: rng) (key_values: [n](k, v)) (op: v -> v -> v) (ne: v) : (rng, hashmap v) =
    hashmap.from_array_hist r key_values op ne

  def hashmap_map 'v 't (g: v -> t) (hmap: hashmap v) : hashmap t =
    hashmap.hashmap_map g hmap

  def update [u] 'v (key_values: [u](k, v)) (hmap: hashmap v) =
    hashmap.update key_values hmap

  def to_array 'v (hmap: hashmap v) : [](k, v) =
    hashmap.to_array hmap

  def size 'k 'v (hmap: hashmap v) =
    hashmap.size hmap

  def member 'v (k: k) (hmap: hashmap v) : bool =
    hashmap.member k hmap

  def not_member 'v (key: k) (hmap: hashmap v) : bool =
    hashmap.not_member key hmap
}
