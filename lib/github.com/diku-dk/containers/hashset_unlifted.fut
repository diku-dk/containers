-- | Static Hashset module
--
-- | This is an implementation of a static hash table using two level
-- hasing https://en.wikipedia.org/wiki/Double_hashing. The modules
-- time complexities assumes only unique keys but the modules does
-- work with duplicate keys.

import "../cpprandom/random"
import "hashmap_unlifted"
import "key"

module type hashset = {
  -- | The key type.
  type k

  -- | The random number generator.
  type rng

  -- | The hashset type.
  type hashset [n] [w] [f] [s]

  -- | Check if a key is member of the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [n] [w] [f] [s] : k -> hashset [n] [w] [f] [s] -> bool

  -- | Check if a key is not member of the hashset
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [n] [w] [f] [s] : k -> hashset [n] [w] [f] [s] -> bool

  -- | Given a random number generator, equality, hash function, and
  -- an array keys construct a hashset. Assumes unique keys but works
  -- with duplicates but too many may lead to unintended behavior
  -- or excessive memory allocations.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [n] : rng -> [n]k -> ?[f][w][s].(rng, hashset [n] [w] [f] [s])

  -- | Convert hashset to an array of keys.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val to_array [n] [w] [f] [s] : hashset [n] [w] [f] [s] -> []k

  -- | The number of elements in the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size [n] [w] [f] [s] : hashset [n] [w] [f] [s] -> i64
}

module hashset (K: key) (E: rng_engine with int.t = K.i)
  : hashset
    with rng = E.rng
    with k = K.k = {
  module hashmap = hashmap K E
  type rng = hashmap.rng
  type k = hashmap.k

  type hashset [n] [w] [f] [s] =
    hashmap.hashmap [n] [w] [f] [s] ()

  def from_array [n]
                 (r: rng)
                 (keys: [n]k) : ?[f][w][s].(rng, hashset [n] [w] [f] [s]) =
    hashmap.from_array_fill r keys ()

  def to_array [n] [w] [f] [s] (set: hashset [n] [w] [f] [s]) : []k =
    hashmap.to_array set
    |> map (.0)

  def size [n] [w] [f] [s] (set: hashset [n] [w] [f] [s]) =
    hashmap.size set

  def member [n] [w] [f] [s]
             (key: k)
             (set: hashset [n] [w] [f] [s]) : bool =
    hashmap.member key set

  def not_member [n] [w] [f] [s]
                 (key: k)
                 (set: hashset [n] [w] [f] [s]) : bool =
    hashmap.not_member key set
}
