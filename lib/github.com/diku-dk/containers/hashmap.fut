-- | Static hashmaps.
--
-- This is an implementation of a static hash table using [two level
-- hashing](https://en.wikipedia.org/wiki/Double_hashing). The modules time
-- complexities assumes only unique keys but the modules does work with
-- duplicate keys.

import "../segmented/segmented"
import "../cpprandom/random"
import "opt"
import "hashkey"
import "array"
import "map"

-- A module type that more directly exposes the implementation details of the
-- hash table.
module type two_level_hashmap = {
  -- | The key type.
  type key

  -- | The context type.
  type ctx

  -- | The hashmap definition.
  type map 'ctx [n] [f] 'v

  -- | Check if a key is member of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> bool

  -- | Check if a key is not member of the hashmap
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> bool

  -- | Look up a value.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val lookup [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> opt v

  -- | Given a key-value array construct a hashmap.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [u] 'v :
    ctx -> [u](key, v) -> ?[n][f].map ctx [n] [f] v

  -- | Create hashmap with default value.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_rep [u] 'v :
    ctx
    -> [u]key
    -> v
    -> ?[n][f].map ctx [n] [f] v

  -- | Create hashmap where duplicates are reduced with an commutative
  -- and associative operation.
  --
  -- **Expected Work:** *O(n + u ✕ W(op))*
  --
  -- **Expected Span:** *O(log n)* (Assuming best case for hist)
  val from_array_hist [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> [u](key, v)
    -> ?[n][f].map ctx [n] [f] v

  -- | Given a key-value array construct a hashmap. If any keys are
  -- duplicates then the function call will never finish. It does less
  -- work than the safe variant.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log n)*
  val unsafe_from_array [n] 'v :
    ctx -> [n](key, v) -> ?[f].map ctx [n] [f] v

  -- | Create hashmap with default value. If any keys are duplicates
  -- then the function call will never finish. It does less work than
  -- the safe variant.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val unsafe_from_array_rep [n] 'v :
    ctx
    -> [n]key
    -> v
    -> ?[f].map ctx [n] [f] v

  -- | Create hashmap where duplicates are reduced with an commutative
  -- and associative operation. If any keys are duplicates then the
  -- function call will never finish.  It does less work than the safe
  -- variant.
  --
  -- **Expected Work:** *O(n + u ✕ W(op))*
  --
  -- **Expected Span:** *O(log n)* (Assuming best case for hist)
  val unsafe_from_array_hist [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> [u](key, v)
    -> ?[n][f].map ctx [n] [f] v

  -- | Compute a histogram using the given key value pairs.
  --
  -- **Work:** *O(n + u ✕ W(op))*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val hist [n] [f] [u] 'v :
    (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> map ctx [n] [f] v

  -- | Map a function over the hashmap values.
  --
  -- **Work:** *O(n ✕ W(g))*
  --
  -- **Span:** *O(S(g))*
  val map [n] [f] 'v 't :
    (g: v -> t) -> map ctx [n] [f] v -> map ctx [n] [f] t

  -- | Map a function over the hashmap values.
  --
  -- **Work:** *O(n ✕ W(g))*
  --
  -- **Span:** *O(S(g))*
  val map_with_key [n] [f] 'v 't :
    (g: key -> v -> t)
    -> map ctx [n] [f] v
    -> map ctx [n] [f] t

  -- | Convert hashmap to an array of key-value pairs.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val to_array [n] [f] 'v : map ctx [n] [f] v -> [n](key, v)

  -- | Updates the value of the hash map using the key with the
  -- smallest index. No new keys are added.
  --
  -- **Work:** *O(n + u)*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val update [n] [f] [u] 'v :
    map ctx [n] [f] v
    -> [u](key, v)
    -> map ctx [n] [f] v

  -- | The number of elements in the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size [n] [f] 'v : map ctx [n] [f] v -> i64

  -- | Gets the context of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val context [n] [f] 'v : map ctx [n] [f] v -> ctx

  -- | Insert new key-value pairs into a hashmap. If a key already
  -- exists in the hashmap, the new value will overwrite the old one.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log (n + u))*
  val insert [n] [f] [u] 'v :
    ctx
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> ?[n'][f'].map ctx [n'] [f'] v

  -- | Insert new key-value pairs into a hashmap, combining values of
  -- duplicate keys using the provided associative and commutative
  -- operation.
  --
  -- **Expected Work:** *O(n' + (n + u) ✕ W(op))*
  --
  -- **Expected Span:** *O(log (n + u))* (Assuming best case for hist)
  val insert_hist [n] [f] [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> ?[n'][f'].map ctx [n'] [f'] v
}

module mk_two_level_hashmap (K: hashkey) (E: rng_engine with int.t = u64)
  : two_level_hashmap
    with key = K.key
    with ctx = K.ctx = {
  module key = K
  module engine = E
  type rng = engine.rng
  type key = key.key
  type~ ctx = key.ctx
  module array = mk_array K E

  type map 'ctx [n] [f] 'v =
    { ctx: ctx
    , keys: [n]key
    , values: [n]v
    , offsets: [f]i64
    , lookup_keys: [f]key
    , level_one_consts: [key.m]u64
    , level_two: [n][2 + key.m]u64
    , rng: rng
    }

  local
  def empty 'v ctx : ?[f].map ctx [0] [f] v =
    { ctx
    , keys = []
    , values = []
    , offsets = []
    , lookup_keys = []
    , level_one_consts = rep 0
    , level_two = []
    , rng = engine.rng_from_seed [123]
    }

  local
  #[inline]
  def generate_consts (n: i64) (r: rng) =
    #[sequential]
    engine.split_rng n r
    |> map engine.rand
    |> unzip
    |> (\(a, b) -> (engine.join_rng a, b))

  local
  def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : (a, [n]a) =
    if length as == 0
    then (ne, as)
    else let res = scan op ne as |> rotate (-1)
         let l = copy res[0]
         let res[0] = ne
         in (l, res)

  local
  #[inline]
  def level_two_hash [n]
                     (ctx: ctx)
                     (level_one_consts: [key.m]u64)
                     (level_two: [n][2 + key.m]u64)
                     (k: key) =
    #[sequential]
    if n == 0
    then 0
    else let o = (key.hash ctx level_one_consts k) %% u64.i64 n
         let (arr, cs) = split level_two[i64.u64 o]
         let o' = i64.u64 arr[0]
         let s = i64.u64 arr[1]
         let y = (key.hash ctx cs k) %% u64.max 1 (u64.i64 s)
         in o' + i64.u64 y

  local
  def loop_body [n] [w]
                (ctx: ctx)
                (old_rng: rng)
                (old_keys: [n](key, i64))
                (old: [w](i64, i64, [key.m]u64)) : ( rng
                                                   , [](key, i64)
                                                   , [](i64, i64, [key.m]u64)
                                                   , [](i64, i64, [key.m]u64)
                                                   ) =
    let (_, old_shape, old_consts) = unzip3 old
    let (flat_size, old_shape_offsets) = old_shape |> exscan (+) 0
    let is =
      map (\(k, o) ->
             i64.u64 (u64.i64 old_shape_offsets[o] + (key.hash ctx old_consts[o] k %% u64.i64 old_shape[o])))
          old_keys
    let has_no_collisions =
      rep 1
      |> hist (+) 0 flat_size is
      |> map (<= 1)
    let flag_idxs =
      map2 (\i j -> if i == 0 then -1 else j) old_shape old_shape_offsets
    let flags =
      scatter (rep false) flag_idxs (rep true)
    let seg_has_no_collision =
      segmented_reduce (&&) true flags has_no_collisions
      |> sized w
    let new_offsets =
      map (\f -> if f then 0 else 1) seg_has_no_collision
      |> scan (+) 0
      |> map2 (\f o -> if f then -1 else o - 1) seg_has_no_collision
    let new_keys =
      filter (\(_, o) -> not seg_has_no_collision[o]) old_keys
      |> map (\(key, o) -> (key, new_offsets[o]))
    let (done, temp_not_done) =
      zip seg_has_no_collision old
      |> partition (\(f, _) -> f)
      |> (\(a, b) -> (map (.1) a, map (.1) b))
    let z = length done
    let s = length temp_not_done
    let (rngs, new_not_done_consts) =
      engine.split_rng s old_rng
      |> map (generate_consts key.m)
      |> unzip
    let not_done =
      map2 (\(i, shp, _) c -> (i, shp + 1, c))
           (temp_not_done :> [s](i64, i64, [key.m]u64))
           new_not_done_consts
    let new_rng = engine.join_rng rngs
    in ( new_rng
       , new_keys
       , not_done :> [s](i64, i64, [key.m]u64)
       , done :> [z](i64, i64, [key.m]u64)
       )

  def unsafe_from_array_rep [n] 'v
                            (ctx: ctx)
                            (keys: [n]key)
                            (ne: v) : ?[f].map ctx [n] [f] v =
    let r = engine.rng_from_seed [123, i32.i64 n]
    let (r, level_one_consts) = generate_consts key.m r
    let is = map (i64.u64 <-< (%% u64.max 1 (u64.i64 n)) <-< key.hash ctx level_one_consts) keys
    let level_one_counts = hist (+) 0i64 n is (rep 1)
    let (idxs, shape) =
      zip (iota n) level_one_counts
      |> filter ((!= 0) <-< (.1))
      |> map (\(i, s) -> (i, s ** 2))
      |> unzip
    let s = length shape
    let shape = sized s shape
    let idxs = sized s idxs
    let (r, init_level_two_consts) =
      engine.split_rng s r
      |> map (generate_consts key.m)
      |> unzip
      |> (\(rngs, b) -> (engine.join_rng rngs, b))
    let level_one_offsets =
      map (i64.bool <-< (!= 0)) level_one_counts
      |> exscan (+) 0
      |> (.1)
      |> map2 (\f o -> if f != 0 then o else 0) level_one_counts
    let init_keys = map2 (\k i -> (k, level_one_offsets[i])) keys is
    let dest = replicate s (0, 0, replicate key.m 0)
    let (final_r, _, _, done, size) =
      loop (old_rng, old_keys, old_not_done, old_done, old_size) =
             (r, init_keys, zip3 idxs shape init_level_two_consts, dest, 0)
      while length old_not_done != 0 do
        let ( new_rng
            , new_keys
            , new_not_done
            , new_done
            ) =
          loop_body ctx old_rng old_keys old_not_done
        let is = map (+ old_size) (indices new_done)
        in ( new_rng
           , new_keys
           , new_not_done
           , scatter old_done is new_done
           , old_size + length new_done
           )
    let done = take size done
    let ( order
        , unordered_level_two_shape
        , unordered_level_two_consts
        ) =
      unzip3 done
    let shape_dest = replicate n 0
    let level_two_shape =
      scatter shape_dest order unordered_level_two_shape
    let (flat_size, level_two_offsets) =
      exscan (+) 0 level_two_shape
      |> (\(a, b) -> (a, map2 (\f o -> if f == 0 then 0 else o) level_two_shape b))
    let consts_dest = replicate n (replicate key.m 0)
    let level_two_consts =
      scatter consts_dest order unordered_level_two_consts
    let level_two: [n][2 + key.m]u64 =
      map3 (\o s cs -> [u64.i64 o, u64.i64 s] ++ cs)
           level_two_offsets
           level_two_shape
           level_two_consts
    let hash2 =
      level_two_hash ctx
                     level_one_consts
                     level_two
    let js = map hash2 keys
    let temp_offsets = hist i64.min i64.highest flat_size js (iota n)
    let key_reordered =
      temp_offsets
      |> filter (\o -> 0 <= o && o < n)
      |> map (\o -> keys[o])
      |> sized n
    let offsets = hist i64.min i64.highest flat_size (map hash2 key_reordered) (indices key_reordered)
    let lookup_keys_dest =
      if n == 0
      then sized flat_size []
      else replicate flat_size keys[0]
    let lookup_keys = scatter lookup_keys_dest js keys
    in { ctx
       , keys = key_reordered
       , lookup_keys = lookup_keys
       , values = replicate n ne
       , offsets = offsets
       , level_one_consts = level_one_consts
       , level_two = level_two
       , rng = final_r
       }

  def from_array_rep [u] 'v
                     (ctx: ctx)
                     (keys: [u]key)
                     (ne: v) : ?[n][f].map ctx [n] [f] v =
    let (_rng, keys) = array.dedup ctx (engine.rng_from_seed [i32.i64 u]) keys
    in unsafe_from_array_rep ctx keys ne

  local
  def offset [n] [f] 'v
             (ctx: ctx)
             (hmap: map ctx [n] [f] v)
             (key: key) : i64 =
    if length hmap.keys == 0
    then -1
    else let i =
           level_two_hash ctx
                          hmap.level_one_consts
                          hmap.level_two
                          key
         in hmap.offsets[i]

  def update [n] [f] [u] 'v
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let js = map (offset hmap.ctx hmap) keys
    let is = hist i64.min i64.highest n js (indices key_values)
    let vs = map2 (\i v -> if i != i64.highest then values[i] else v) is hmap.values
    in hmap with values = vs

  def from_array [u] 'v
                 (ctx: ctx)
                 (key_values: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
    if u == 0
    then (empty ctx)
    else let (keys, values) = unzip key_values
         let hmap = from_array_rep ctx keys values[0]
         in update hmap key_values

  def unsafe_from_array [n] 'v
                        (ctx: ctx)
                        (key_values: [n](key, v)) : ?[f].(map ctx [n] [f] v) =
    if n == 0
    then empty ctx :> map ctx [n] [0] v
    else let (keys, values) = unzip key_values
         let hmap = unsafe_from_array_rep ctx keys values[0]
         in update hmap key_values

  def hist [n] [f] [u] 'v
           (op: v -> v -> v)
           (ne: v)
           (hmap: map ctx [n] [f] v)
           (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let is = map (offset hmap.ctx hmap) keys
    let vs = reduce_by_index (copy hmap.values) op ne is values
    in hmap with values = vs

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
    let keys = map (.0) key_values
    let hmap = from_array_rep ctx keys ne
    in hist op ne hmap key_values

  def unsafe_from_array_hist [u] 'v
                             (ctx: ctx)
                             (op: v -> v -> v)
                             (ne: v)
                             (key_values: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
    let keys = map (.0) key_values
    let hmap = unsafe_from_array_rep ctx keys ne
    in hist op ne hmap key_values

  def to_array [n] [f] 'v (hmap: map ctx [n] [f] v) : [](key, v) =
    zip hmap.keys hmap.values

  def size [n] [f] 'v (hmap: map ctx [n] [f] v) =
    length hmap.keys

  def context [n] [f] 'v (hmap: map ctx [n] [f] v) =
    hmap.ctx

  local
  def lookup_idx [n] [f] 'v
                 (ctx: ctx)
                 (k: key)
                 (hmap: map ctx [n] [f] v) : i64 =
    if length hmap.keys == 0
    then -1
    else let j = i64.min (length hmap.keys - 1) (i64.max 0 (offset ctx hmap k))
         let k' = hmap.keys[j]
         in if key.eq hmap.ctx k' ctx k then j else -1

  def member [n] [f] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] v) : bool =
    if n == 0
    then false
    else let i =
           level_two_hash ctx
                          hmap.level_one_consts
                          hmap.level_two
                          k
         in key.eq hmap.ctx hmap.lookup_keys[i] ctx k

  def not_member [n] [f] 'v
                 (ctx: ctx)
                 (key: key)
                 (hmap: map ctx [n] [f] v) : bool =
    not (member ctx key hmap)

  def lookup [n] [f] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] v) : opt v =
    let j = lookup_idx ctx k hmap
    in if j == -1
       then #none
       else some hmap.values[j]

  def insert [n] [f] [u] 'v
             (ctx: ctx)
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    if n == 0 && u == 0
    then hmap
    else let key_values' = to_array hmap
         let (rng, keys) =
           key_values
           |> map (.0)
           |> filter (\k -> not_member ctx k hmap)
           |> array.dedup ctx (hmap.rng)
         let keys' = map (.0) key_values'
         let filler = if u < n then key_values'[0].1 else key_values[0].1
         let keys'' = keys ++ keys'
         let hmap' = unsafe_from_array_rep ctx keys'' filler
         in update hmap' (key_values ++ key_values') with rng = rng

  def insert_hist [n] [f] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map ctx [n] [f] v)
                  (key_values: [u](key, v)) =
    let key_values' = to_array hmap
    let (rng, keys) =
      key_values
      |> map (.0)
      |> filter (\k -> not_member ctx k hmap)
      |> array.dedup ctx (hmap.rng)
    let keys' = map (.0) key_values'
    let keys'' = keys ++ keys'
    let hmap' = unsafe_from_array_rep ctx keys'' ne
    in hist op ne hmap' (key_values ++ key_values') with rng = rng

  def map_with_key [n] [f] 'v 't
                   (g: key -> v -> t)
                   (hmap: map ctx [n] [f] v) : map ctx [n] [f] t =
    let vs = map2 g hmap.keys hmap.values
    in { ctx = hmap.ctx
       , keys = hmap.keys
       , lookup_keys = hmap.lookup_keys
       , values = vs
       , offsets = hmap.offsets
       , level_one_consts = hmap.level_one_consts
       , level_two = hmap.level_two
       , rng = hmap.rng
       }

  def map [n] [f] 'v 't
          (g: v -> t)
          (hmap: map ctx [n] [f] v) : map ctx [n] [f] t =
    map_with_key (\_ v -> g v) hmap
}

-- | Create an implementation of `map`@mtype using two level hash tables.
module mk_hashmap (K: hashkey) (E: rng_engine with int.t = u64)
  : map
    with key = K.key
    with ctx = K.ctx = {
  module hashmap = mk_two_level_hashmap K E
  type key = hashmap.key
  type ctx = hashmap.ctx

  type~ map [n] 'v =
    ?[f].hashmap.map ctx [n] [f] v

  def from_array [n] 'v (ctx: ctx) (key_values: [n](key, v)) : ?[m].map [m] v =
    hashmap.from_array ctx key_values

  def from_array_rep [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : ?[m].map [m] v =
    hashmap.from_array_rep ctx keys ne

  def hist [n] [u] 'v
           (op: v -> v -> v)
           (ne: v)
           (hmap: map [n] v)
           (key_values: [u](key, v)) =
    hashmap.hist op ne hmap key_values

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n].map [n] v =
    hashmap.from_array_hist ctx op ne key_values

  def unsafe_from_array [n] 'v (ctx: ctx) (key_values: [n](key, v)) : map [n] v =
    hashmap.unsafe_from_array ctx key_values

  def unsafe_from_array_rep [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : map [n] v =
    hashmap.unsafe_from_array_rep ctx keys ne

  def unsafe_from_array_hist [u] 'v
                             (ctx: ctx)
                             (op: v -> v -> v)
                             (ne: v)
                             (key_values: [u](key, v)) : ?[n].map [n] v =
    hashmap.unsafe_from_array_hist ctx op ne key_values

  def map [n] 'v 't (g: v -> t) (hmap: map [n] v) : map [n] t =
    hashmap.map g hmap

  def map_with_key [n] 'v 't (g: key -> v -> t) (hmap: map [n] v) : map [n] t =
    hashmap.map_with_key g hmap

  def update [n] [u] 'v (hmap: map [n] v) (key_values: [u](key, v)) =
    hashmap.update hmap key_values

  def to_array [n] 'v (hmap: map [n] v) : [n](key, v) =
    hashmap.to_array hmap

  def size [n] 'v (hmap: map [n] v) =
    hashmap.size hmap

  def member [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : bool =
    hashmap.member ctx k hmap

  def not_member [n] 'v (ctx: ctx) (key: key) (hmap: map [n] v) : bool =
    hashmap.not_member ctx key hmap

  def lookup [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : opt v =
    hashmap.lookup ctx k hmap

  def context hmap =
    hashmap.context hmap

  def insert [n] [u] 'v
             (ctx: ctx)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    hashmap.insert ctx hmap key_values

  def insert_hist [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map [n] v)
                  (key_values: [u](key, v)) =
    hashmap.insert_hist ctx op ne hmap key_values
}
