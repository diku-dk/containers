-- | Static Hashset module
--
-- | This is an implementation of a static hash table using two level
-- hasing https://en.wikipedia.org/wiki/Double_hashing. The modules
-- time complexities assumes only unique keys but the modules does
-- work with duplicate keys.

import "../cpprandom/random"
import "static_hashmap"

module type static_hashset = {
  -- | Used for random number generation.
  module engine: rng_engine

  -- | The hashset type.
  type^ hashset [m] [n] [w] [f] [s] 'k

  -- | Check if a key is member of the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [m] [n] [w] [f] [s] 'k : k -> hashset [m] [n] [w] [f] [s] k -> bool

  -- | Check if a key is not member of the hashset
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [m] [n] [w] [f] [s] 'k : k -> hashset [m] [n] [w] [f] [s] k -> bool

  -- | Given a random number generator, equality, hash function, and
  -- an array keys construct a hashset. Assumes unique keys but works
  -- with duplicates but too many may lead to unintended behavior
  -- or excessive memory allocations.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [n] [m] 'k :
    engine.rng
    -> (k -> k -> bool)
    -> ([m]engine.int.t -> k -> i64)
    -> [n]k
    -> ?[f][w][s].(engine.rng, hashset [m] [n] [w] [f] [s] k)

  -- | Convert hashset to an array of keys.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)* 
  val to_array [m] [n] [w] [f] [s] 'k : hashset [m] [n] [w] [f] [s] k -> []k

  -- | The number of elements in the hashset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)* 
  val size [m] [n] [w] [f] [s] 'k : hashset [m] [n] [w] [f] [s] k -> i64
}

module static_hashset (R: rng_engine) : static_hashset = {
  module hashmap = static_hashmap R
  module engine = hashmap.engine
  type rng = engine.rng
  type int = engine.int.t
  module int = engine.int

  type^ hashset [m] [n] [w] [f] [s] 'k =
    hashmap.hashmap [m] [n] [w] [f] [s] k ()


  def from_array [n] [m] 'k
                 (r: rng)
                 (eq: k -> k -> bool)
                 (hash: [m]int -> k -> i64)
                 (keys: [n]k) : ?[f][w][s].(rng, hashset [m] [n] [w] [f] [s] k) =
    hashmap.from_array_fill r eq hash keys ()
    
  def to_array [m] [n] [w] [f] [s] 'k (set: hashset [m] [n] [w] [f] [s] k) : []k =
    hashmap.to_array set
    |> map (.0)

  def size [m] [n] [w] [f] [s] 'k (set: hashset [m] [n] [w] [f] [s] k) =
    hashmap.size set

  def member [m] [n] [w] [f] [s] 'k
             (key: k)
             (set: hashset [m] [n] [w] [f] [s] k) : bool =
    hashmap.member key set
                   
  def not_member [m] [n] [w] [f] [s] 'k
                 (key: k)
                 (set: hashset [m] [n] [w] [f] [s] k) : bool =
    hashmap.not_member key set
}
