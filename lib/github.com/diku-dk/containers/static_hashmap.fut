-- | Static Hashset module
--
-- | This is an implementation of a static hash table using two level
-- hasing https://en.wikipedia.org/wiki/Double_hashing. The modules
-- time complexities assumes only unique keys but the modules does
-- work with duplicate keys.

import "../segmented/segmented"
import "../cpprandom/random"

module type static_hashmap = {
  -- | Used for random number generation.
  module engine: rng_engine

  -- | The hashmap type.
  type^ hashmap [m] [n] [w] [f] [s] 'k 'v

  -- | Check if a key is member of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [m] [n] [w] [f] [s] 'k 'v : k -> hashmap [m] [n] [w] [f] [s] k v -> bool

  -- | Check if a key is not member of the hashmap
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [m] [n] [w] [f] [s] 'k 'v : k -> hashmap [m] [n] [w] [f] [s] k v -> bool

  -- | Given a random number generator, equality, hash function, and
  -- a key-value array construct a hashmap. Assumes unique keys but
  -- works with duplicates but too many may lead to unintended
  -- behavior or excessive memory allocations. Keys with the smallest
  -- hash will be priotized.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [n] [m] 'k 'v :
    engine.rng
    -> (k -> k -> bool)
    -> ([m]engine.int.t -> k -> i64)
    -> [n](k, v)
    -> ?[f][w][s].(engine.rng, hashmap [m] [n] [w] [f] [s] k v)

  -- | Create hashmap with default value.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_fill [n] [m] 'k 'v :
    engine.rng
    -> (k -> k -> bool)
    -> ([m]engine.int.t -> k -> i64)
    -> [n]k
    -> v
    -> ?[f][w][s].(engine.rng, hashmap [m] [n] [w] [f] [s] k v)

  -- | Create hashmap where duplicates are reduced with an commutative
  -- and associative operation.
  --
  -- **Expected Work:** *O(s + n ✕ W(op))*
  --
  -- **Expected Span:** *O(log n)* (Assuming best case for hist)
  val from_array_hist [n] [m] 'k 'v :
    engine.rng
    -> (k -> k -> bool)
    -> ([m]engine.int.t -> k -> i64)
    -> [n](k, v)
    -> (v -> v -> v)
    -> v
    -> ?[f][w][s].(engine.rng, hashmap [m] [n] [w] [f] [s] k v)

  -- | Compute a histogram using the given key value pairs.
  --
  -- Same asymptotics as the hist SOAC.
  val hashmap_hist [m] [n] [w] [f] [s] [u] 'k 'v :
    hashmap [m] [n] [w] [f] [s] k v
    -> [u](k, v)
    -> (v -> v -> v)
    -> v
    -> hashmap [m] [n] [w] [f] [s] k v

  -- | Map a function over the hashmap values.
  --
  -- **Work:** *O(s ✕ W(g))*
  --
  -- **Span:** *O(s(g))*
  val hashmap_map [m] [n] [w] [f] [s] 'k 'v 't :
    (g: v -> t)
    -> hashmap [m] [n] [w] [f] [s] k v
    -> hashmap [m] [n] [w] [f] [s] k t

  -- | Convert hashmap to an array of key-value pairs.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val to_array [m] [n] [w] [f] [s] 'k 'v : hashmap [m] [n] [w] [f] [s] k v -> [](k, v)

  -- | Updates the value of the hash map using the key with the
  -- smallest index.
  --
  -- **Work:** *O(s + u)*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val update [m] [n] [w] [f] [s] [u] 'k 'v :
    [u](k, v)
    -> hashmap [m] [n] [w] [f] [s] k v
    -> hashmap [m] [n] [w] [f] [s] k v

  -- | The number of elements in the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size [m] [n] [w] [f] [s] 'k 'v : hashmap [m] [n] [w] [f] [s] k v -> i64
}

module static_hashmap (R: rng_engine) : static_hashmap = {
  module engine = R
  type rng = engine.rng
  type int = engine.int.t
  module int = engine.int

  type^ hashmap [m] [n] [w] [f] [s] 'k 'v =
    { keys: [s]k
    , values: [s]v
    , offsets: [f]i64
    , level_one_consts: [m]int
    , level_one_offsets: [n]i64
    , level_two_offsets: [w]i64
    , level_two_consts: [w][m]int
    , level_two_shape: [w]i64
    , hash: [m]int -> k -> i64
    , eq: k -> k -> bool
    }

  local
  def generate_consts (n: i64) (r: rng) =
    engine.split_rng n r
    |> map engine.rand
    |> unzip
    |> (\(a, b) -> (engine.join_rng a, b))

  local
  def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) =
    if length as == 0
    then (ne, as)
    else let res = scan op ne as |> rotate (-1)
         let l = copy res[0]
         let res[0] = ne
         in (l, res)

  local
  def level_two_hash 'k [m]
                     (hash: [m]int -> k -> i64)
                     (level_one_consts: [m]int)
                     (n: i64)
                     (level_one_offsets: []i64)
                     (level_two_offsets: []i64)
                     (level_two_consts: [][m]int)
                     (level_two_shape: []i64)
                     (key: k) =
    if n == 0
    then 0
    else let x = hash level_one_consts key % n
         let o = level_one_offsets[x]
         let cs = level_two_consts[o]
         let s = level_two_shape[o]
         let y = hash cs key % s
         in level_two_offsets[o] + y

  local type cmp 'a = #id | #elem a | #collision

  local
  def equiv 'a (eq: a -> a -> bool) (a: cmp a) (b: cmp a) : cmp a =
    match (a, b)
    case (_, #id) -> a
    case (#id, _) -> b
    case (_, #collision) -> #collision
    case (#collision, _) -> #collision
    case (#elem a', #elem b') ->
      if a' `eq` b'
      then #elem a'
      else #collision

  local
  def is_not_collision 'a (a: cmp a) : bool =
    match a
    case #elem _ -> true
    case #id -> true
    case #collision -> false

  local
  def loop_body [n] [w] [m] 'k
                (eq: k -> k -> bool)
                (hash: [m]int -> k -> i64)
                (old_rng: rng)
                (old_keys: [n](k, i64))
                (old: [w](i64, i64, [m]int)) : ( rng
                                               , [](k, i64)
                                               , [](i64, i64, [m]int)
                                               , [](i64, i64, [m]int)
                                               ) =
    let (_, old_shape, old_consts) = unzip3 old
    let (flat_size, old_shape_offsets) = old_shape |> exscan (+) 0
    let is =
      map (\(k, o) ->
             old_shape_offsets[o] + (hash old_consts[o] k % old_shape[o]))
          old_keys
    let has_no_collisions =
      map (\(k, _) -> #elem k) old_keys
      |> hist (equiv eq) #id flat_size is
      |> map is_not_collision
    let flag_idxs =
      map2 (\i j -> if i == 0 then -1 else j) old_shape old_shape_offsets
    let flags =
      scatter (replicate flat_size false) flag_idxs (replicate w true)
    let seg_has_no_collision =
      segmented_reduce (&&) true flags has_no_collisions
      |> sized w
    let (_, new_offsets) =
      map (\f -> if f then 0 else 1) seg_has_no_collision
      |> exscan (+) 0
    let new_keys =
      filter (\(_, o) -> not seg_has_no_collision[o]) old_keys
      |> map (\(k, o) -> (k, new_offsets[o]))
    let (done, temp_not_done) =
      zip seg_has_no_collision old
      |> partition (\(f, _) -> f)
      |> (\(a, b) -> (map (.1) a, map (.1) b))
    let z = length done
    let s = length temp_not_done
    let (rngs, new_not_done_consts) =
      engine.split_rng s old_rng
      |> map (generate_consts m)
      |> unzip
    let not_done =
      map2 (\(i, shp, _) c -> (i, shp + 1, c))
           (temp_not_done :> [s](i64, i64, [m]int))
           new_not_done_consts
    let new_rng = engine.join_rng rngs
    in ( new_rng
       , new_keys
       , not_done :> [s](i64, i64, [m]int)
       , done :> [z](i64, i64, [m]int)
       )

  def from_array [n] [m] 'k 'v
                 (r: rng)
                 (eq: k -> k -> bool)
                 (hash: [m]int -> k -> i64)
                 (key_values: [n](k, v)) : ?[f][w][s].(rng, hashmap [m] [n] [w] [f] [s] k v) =
    let (keys, values) = unzip key_values
    let (r, level_one_consts) = generate_consts m r
    let is = map ((% i64.max 1 n) <-< hash level_one_consts) keys
    let level_one_counts_squared =
      -- This comes from the unique keys assumption, should be changed
      -- to account for many duplicates by estimating duplicates in
      -- they keys.
      replicate n 1 |> hist (+) 0i64 n is
      |> map (** 2)
    let shape = filter (!= 0) level_one_counts_squared
    let s = length shape
    let shape = sized s shape
    let level_one_offsets =
      map (i64.bool <-< (!= 0)) level_one_counts_squared
      |> exscan (+) 0
      |> (.1)
      |> map2 (\f o -> if f != 0 then o else 0) level_one_counts_squared
    let (r, init_level_two_consts) =
      engine.split_rng s r
      |> map (generate_consts m)
      |> unzip
      |> (\(rngs, b) -> (engine.join_rng rngs, b))
    let init_keys = map2 (\k i -> (k, level_one_offsets[i])) keys is
    let (final_r, _, _, done) =
      loop ( old_rng
           , old_keys
           , old_not_done
           , old_done
           ) =
             ( r
             , init_keys
             , zip3 (iota s) shape init_level_two_consts
             , []
             )
      while length old_not_done != 0 do
        let ( new_rng
            , new_keys
            , new_not_done
            , new_done
            ) =
          loop_body eq hash old_rng old_keys old_not_done
        in ( new_rng
           , new_keys
           , new_not_done
           , old_done ++ new_done
           )
    let ( order
        , unordered_level_two_shape
        , unordered_level_two_consts
        ) =
      unzip3 done
    let shape_dest = replicate s 0
    let level_two_shape =
      scatter shape_dest order unordered_level_two_shape
    let (flat_size, level_two_offsets) =
      exscan (+) 0 level_two_shape
      |> (\(a, b) -> (a, map2 (\s o -> if s != 0 then o else 0) level_two_shape b))
    let consts_dest = replicate s (replicate m (int.u8 0))
    let level_two_consts =
      scatter consts_dest order unordered_level_two_consts
    let hash2 =
      level_two_hash hash
                     level_one_consts
                     n
                     level_one_offsets
                     level_two_offsets
                     level_two_consts
                     level_two_shape
    let js = map hash2 keys
    let temp_offsets = hist i64.min i64.highest flat_size js (iota n)
    let (simp_keys, simp_values) =
      temp_offsets
      |> filter (\o -> 0 <= o && o < n)
      |> map (\o -> (keys[o], values[o]))
      |> unzip
    let offsets = hist i64.min i64.highest flat_size (map hash2 simp_keys) (indices simp_keys)
    in ( final_r
       , { keys = simp_keys
         , values = simp_values
         , offsets = offsets
         , level_one_consts = level_one_consts
         , level_one_offsets = level_one_offsets
         , level_two_offsets = level_two_offsets
         , level_two_consts = level_two_consts
         , level_two_shape = level_two_shape
         , eq = eq
         , hash = hash
         }
       )

  def from_array_fill [n] [m] 'k 'v
                      (r: rng)
                      (eq: k -> k -> bool)
                      (hash: [m]int -> k -> i64)
                      (keys: [n]k)
                      (ne: v) : ?[f][w][s].(rng, hashmap [m] [n] [w] [f] [s] k v) =
    zip keys (replicate n ne)
    |> from_array r eq hash

  local
  def offset [m] [n] [w] [f] [s] 'k 'v
             (hmap: hashmap [m] [n] [w] [f] [s] k v)
             (key: k) : i64 =
    if length hmap.keys == 0
    then -1
    else let i =
           level_two_hash hmap.hash
                          hmap.level_one_consts
                          n
                          hmap.level_one_offsets
                          hmap.level_two_offsets
                          hmap.level_two_consts
                          hmap.level_two_shape
                          key
         in hmap.offsets[i]

  def hashmap_hist [m] [n] [w] [f] [s] [u] 'k 'v
                   (hmap: hashmap [m] [n] [w] [f] [s] k v)
                   (key_values: [u](k, v))
                   (op: v -> v -> v)
                   (ne: v) =
    let (keys, values) = unzip key_values
    let is = map (offset hmap) keys
    let vs = reduce_by_index (copy hmap.values) op ne is values
    in hmap with values = vs

  def from_array_hist [n] [m] 'k 'v
                      (r: rng)
                      (eq: k -> k -> bool)
                      (hash: [m]int -> k -> i64)
                      (key_values: [n](k, v))
                      (op: v -> v -> v)
                      (ne: v) : ?[f][w][s].(rng, hashmap [m] [n] [w] [f] [s] k v) =
    let keys = map (.0) key_values
    let (r, hmap) = from_array_fill r eq hash keys ne
    in (r, hashmap_hist hmap key_values op ne)

  def hashmap_map [m] [n] [w] [f] [s] 'k 'v 't
                  (g: v -> t)
                  (hmap: hashmap [m] [n] [w] [f] [s] k v) : hashmap [m] [n] [w] [f] [s] k t =
    let vs = map g hmap.values
    in { keys = hmap.keys
       , values = vs
       , offsets = hmap.offsets
       , level_one_consts = hmap.level_one_consts
       , level_one_offsets = hmap.level_one_offsets
       , level_two_offsets = hmap.level_two_offsets
       , level_two_consts = hmap.level_two_consts
       , level_two_shape = hmap.level_two_shape
       , eq = hmap.eq
       , hash = hmap.hash
       }

  def update [m] [n] [w] [f] [s] [u] 'k 'v
             (key_values: [u](k, v))
             (hmap: hashmap [m] [n] [w] [f] [s] k v) =
    let (keys, values) = unzip key_values
    let js = map (offset hmap) keys
    let is = hist i64.min i64.highest s js (iota u)
    let vs = map2 (\i v -> if i != i64.highest then values[i] else v) is hmap.values
    in hmap with values = vs

  def to_array [m] [n] [w] [f] [s] 'k 'v (hmap: hashmap [m] [n] [w] [f] [s] k v) : [](k, v) =
    zip hmap.keys hmap.values

  def size [m] [n] [w] [f] [s] 'k 'v (hmap: hashmap [m] [n] [w] [f] [s] k v) =
    length hmap.keys

  def member [m] [n] [w] [f] [s] 'k 'v
             (key: k)
             (hmap: hashmap [m] [n] [w] [f] [s] k v) : bool =
    if length hmap.keys == 0
    then false
    else let j = i64.min (length hmap.keys - 1) (i64.max 0 (offset hmap key))
         let key' = hmap.keys[j]
         in hmap.eq key' key

  def not_member [m] [n] [w] [f] [s] 'k 'v
                 (key: k)
                 (hmap: hashmap [m] [n] [w] [f] [s] k v) : bool =
    not (key `member` hmap)
}
