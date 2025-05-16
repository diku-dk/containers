-- | Static hashmaps.
--
-- This is an implementation of a static hash table using [two level
-- hashing](https://en.wikipedia.org/wiki/Double_hashing). The modules time
-- complexities assumes only unique keys but the modules does work with
-- duplicate keys.

import "../segmented/segmented"
import "../cpprandom/random"
import "opt"
import "key"
import "array"

module type hashmap_unlifted = {
  -- | The key type.
  type k

  -- | The context type.
  type ctx

  -- | The random number generator.
  type rng

  -- | The hashmap definitionen.
  type hashmap 'ctx [n] [f] 'v

  -- | Check if a key is member of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [n] [f] 'v : ctx -> k -> hashmap ctx [n] [f] v -> bool

  -- | Check if a key is not member of the hashmap
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [n] [f] 'v : ctx -> k -> hashmap ctx [n] [f] v -> bool

  -- | Look up a value.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val lookup [n] [f] 'v : ctx -> k -> hashmap ctx [n] [f] v -> opt v

  -- | Given a key-value array construct a hashmap.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [u] 'v :
    ctx -> rng -> [u](k, v) -> ?[n][f].(rng, hashmap ctx [n] [f] v)

  -- | Create hashmap with default value.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_replicate [u] 'v :
    ctx
    -> rng
    -> [u]k
    -> v
    -> ?[n][f].(rng, hashmap ctx [n] [f] v)

  -- | Create hashmap where duplicates are reduced with an commutative
  -- and associative operation.
  --
  -- **Expected Work:** *O(n + u ✕ W(op))*
  --
  -- **Expected Span:** *O(log n)* (Assuming best case for hist)
  val from_array_hist [u] 'v :
    ctx
    -> rng
    -> (v -> v -> v)
    -> v
    -> [u](k, v)
    -> ?[n][f].(rng, hashmap ctx [n] [f] v)

  -- | Compute a histogram using the given key value pairs.
  --
  -- **Work:** *O(n + u ✕ W(op))*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val hist [n] [f] [u] 'v :
    (v -> v -> v)
    -> v
    -> hashmap ctx [n] [f] v
    -> [u](k, v)
    -> hashmap ctx [n] [f] v

  -- | Map a function over the hashmap values.
  --
  -- **Work:** *O(n ✕ W(g))*
  --
  -- **Span:** *O(S(g))*
  val map [n] [f] 'v 't :
    (g: v -> t) -> hashmap ctx [n] [f] v -> hashmap ctx [n] [f] t

  -- | Map a function over the hashmap values.
  --
  -- **Work:** *O(n ✕ W(g))*
  --
  -- **Span:** *O(S(g))*
  val map_with_key [n] [f] 'v 't :
    (g: k -> v -> t)
    -> hashmap ctx [n] [f] v
    -> hashmap ctx [n] [f] t

  -- | Convert hashmap to an array of key-value pairs.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val to_array [n] [f] 'v : hashmap ctx [n] [f] v -> [](k, v)

  -- | Updates the value of the hash map using the key with the
  -- smallest index.
  --
  -- **Work:** *O(n + u)*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val update [n] [f] [u] 'v :
    [u](k, v)
    -> hashmap ctx [n] [f] v
    -> hashmap ctx [n] [f] v

  -- | The number of elements in the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size [n] [f] 'v : hashmap ctx [n] [f] v -> i64
}

module mk_hashmap_unlifted (K: key) (E: rng_engine with int.t = u64)
  : hashmap_unlifted
    with rng = E.rng
    with k = K.k
    with ctx = K.ctx = {
  module key = K
  module engine = E
  type rng = engine.rng
  type k = key.k
  type~ ctx = key.ctx
  module array = mk_array K E

  type hashmap 'ctx [n] [f] 'v =
    { ctx: ctx
    , keys: [n]k
    , values: [n]v
    , offsets: [f]i64
    , lookup_keys: [f]k
    , level_one_consts: [key.m]u64
    , level_two: [n][2 + key.m]u64
    }

  local
  def empty 'v ctx : ?[n][f].hashmap ctx [n] [f] v =
    { ctx
    , keys = []
    , values = []
    , offsets = []
    , lookup_keys = []
    , level_one_consts = replicate key.m 0
    , level_two = []
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
                     (k: k) =
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
                (old_keys: [n](k, i64))
                (old: [w](i64, i64, [key.m]u64)) : ( rng
                                                   , [](k, i64)
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
      replicate n 1
      |> hist (+) 0 flat_size is
      |> map (<= 1)
    let flag_idxs =
      map2 (\i j -> if i == 0 then -1 else j) old_shape old_shape_offsets
    let flags =
      scatter (replicate flat_size false) flag_idxs (replicate w true)
    let seg_has_no_collision =
      segmented_reduce (&&) true flags has_no_collisions
      |> sized w
    let new_offsets =
      map (\f -> if f then 0 else 1) seg_has_no_collision
      |> scan (+) 0
      |> map2 (\f o -> if f then -1 else o - 1) seg_has_no_collision
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

  def unsafe_from_array_replicate [n] 'v
                                  (ctx: ctx)
                                  (r: rng)
                                  (keys: [n]k)
                                  (ne: v) : ?[f].(rng, hashmap ctx [n] [f] v) =
    let (r, level_one_consts) = generate_consts key.m r
    let is = map (i64.u64 <-< (%% u64.max 1 (u64.i64 n)) <-< key.hash ctx level_one_consts) keys
    let level_one_counts =
      replicate n 1
      |> hist (+) 0i64 n is
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
    in ( final_r
       , { ctx
         , keys = key_reordered
         , lookup_keys = lookup_keys
         , values = replicate n ne
         , offsets = offsets
         , level_one_consts = level_one_consts
         , level_two = level_two
         }
       )

  def from_array_replicate [u] 'v
                           (ctx: ctx)
                           (r: rng)
                           (keys: [u]k)
                           (ne: v) : ?[n][f].(rng, hashmap ctx [n] [f] v) =
    let (r, keys) = array.dedup ctx r keys
    in unsafe_from_array_replicate ctx r keys ne

  local
  def offset [n] [f] 'v
             (ctx: ctx)
             (hmap: hashmap ctx [n] [f] v)
             (key: k) : i64 =
    if length hmap.keys == 0
    then -1
    else let i =
           level_two_hash ctx
                          hmap.level_one_consts
                          hmap.level_two
                          key
         in hmap.offsets[i]

  def update [n] [f] [u] 'v
             (key_values: [u](k, v))
             (hmap: hashmap ctx [n] [f] v) =
    let (keys, values) = unzip key_values
    let js = map (offset hmap.ctx hmap) keys
    let is = hist i64.min i64.highest n js (indices key_values)
    let vs = map2 (\i v -> if i != i64.highest then values[i] else v) is hmap.values
    in hmap with values = vs

  def from_array [u] 'v
                 (ctx: ctx)
                 (r: rng)
                 (key_values: [u](k, v)) : ?[n][f].(rng, hashmap ctx [n] [f] v) =
    if u == 0
    then (r, empty ctx)
    else let (keys, values) = unzip key_values
         let (r, hmap) = from_array_replicate ctx r keys values[0]
         let final_hmap = update key_values hmap
         in (r, final_hmap)

  def hist [n] [f] [u] 'v
           (op: v -> v -> v)
           (ne: v)
           (hmap: hashmap ctx [n] [f] v)
           (key_values: [u](k, v)) =
    let (keys, values) = unzip key_values
    let is = map (offset hmap.ctx hmap) keys
    let vs = reduce_by_index (copy hmap.values) op ne is values
    in hmap with values = vs

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (r: rng)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](k, v)) : ?[n][f].(rng, hashmap ctx [n] [f] v) =
    let keys = map (.0) key_values
    let (r, hmap) = from_array_replicate ctx r keys ne
    in (r, hist op ne hmap key_values)

  def map_with_key [n] [f] 'v 't
                   (g: k -> v -> t)
                   (hmap: hashmap ctx [n] [f] v) : hashmap ctx [n] [f] t =
    let vs = map2 g hmap.keys hmap.values
    in { ctx = hmap.ctx
       , keys = hmap.keys
       , lookup_keys = hmap.lookup_keys
       , values = vs
       , offsets = hmap.offsets
       , level_one_consts = hmap.level_one_consts
       , level_two = hmap.level_two
       }

  def map [n] [f] 'v 't
          (g: v -> t)
          (hmap: hashmap ctx [n] [f] v) : hashmap ctx [n] [f] t =
    map_with_key (\_ v -> g v) hmap

  def to_array [n] [f] 'v (hmap: hashmap ctx [n] [f] v) : [](k, v) =
    zip hmap.keys hmap.values

  def size [n] [f] 'v (hmap: hashmap ctx [n] [f] v) =
    length hmap.keys

  local
  def lookup_idx [n] [f] 'v
                 (ctx: ctx)
                 (k: k)
                 (hmap: hashmap ctx [n] [f] v) : i64 =
    if length hmap.keys == 0
    then -1
    else let j = i64.min (length hmap.keys - 1) (i64.max 0 (offset ctx hmap k))
         let k' = hmap.keys[j]
         in if key.eq hmap.ctx k' ctx k then j else -1

  def member [n] [f] 'v
             (ctx: ctx)
             (k: k)
             (hmap: hashmap ctx [n] [f] v) : bool =
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
                 (key: k)
                 (hmap: hashmap ctx [n] [f] v) : bool =
    not (member ctx key hmap)

  def lookup [n] [f] 'v
             (ctx: ctx)
             (k: k)
             (hmap: hashmap ctx [n] [f] v) : opt v =
    let j = lookup_idx ctx k hmap
    in if j == -1
       then #none
       else some hmap.values[j]

  def union [n] [f] [n'] [f'] 'v
            (ctx: ctx)
            (r: rng)
            (hmap: hashmap ctx [n] [f] v)
            (hmap': hashmap ctx [n'] [f'] v) =
    if n == 0
    then (r, hmap')
    else if n' == 0
    then (r, hmap)
    else let key_values = to_array hmap
         let key_values' = to_array hmap'
         in from_array ctx r (key_values ++ key_values')
}

module type hashmap = {
  type k

  type ctx

  type rng

  type~ hashmap 'v

  val member 'v : ctx -> k -> hashmap v -> bool

  val not_member 'v : ctx -> k -> hashmap v -> bool

  val lookup 'v : ctx -> k -> hashmap v -> opt v

  val from_array [n] 'v :
    ctx -> rng -> [n](k, v) -> (rng, hashmap v)

  val from_array_replicate [n] 'v :
    ctx -> rng -> [n]k -> v -> (rng, hashmap v)

  val from_array_hist [n] 'v :
    ctx -> rng -> (v -> v -> v) -> v -> [n](k, v) -> (rng, hashmap v)

  val hist [n] 'v :
    (v -> v -> v) -> v -> hashmap v -> [n](k, v) -> hashmap v

  val map_with_key [n] [f] 'v 't :
    (g: k -> v -> t)
    -> hashmap v
    -> hashmap t

  val map 'v 't : (g: v -> t) -> hashmap v -> hashmap t

  val to_array 'v : hashmap v -> [](k, v)

  val update [u] 'v :
    [u](k, v)
    -> hashmap v
    -> hashmap v

  val size 'v : hashmap v -> i64
}

module mk_hashmap (K: key) (E: rng_engine with int.t = u64)
  : hashmap
    with rng = E.rng
    with k = K.k
    with ctx = K.ctx = {
  module hashmap = mk_hashmap_unlifted K E
  type rng = hashmap.rng
  type k = hashmap.k
  type ctx = hashmap.ctx

  type~ hashmap 'v =
    ?[n][f].hashmap.hashmap ctx [n] [f] v

  def from_array [n] 'v (ctx: ctx) (r: rng) (key_values: [n](k, v)) : (rng, hashmap v) =
    hashmap.from_array ctx r key_values

  def from_array_replicate [n] 'v (ctx: ctx) (r: rng) (keys: [n]k) (ne: v) : (rng, hashmap v) =
    hashmap.from_array_replicate ctx r keys ne

  def hist [n] 'v
           (op: v -> v -> v)
           (ne: v)
           (hmap: hashmap v)
           (key_values: [n](k, v)) =
    hashmap.hist op ne hmap key_values

  def from_array_hist [n] 'v
                      (ctx: ctx)
                      (r: rng)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [n](k, v)) : (rng, hashmap v) =
    hashmap.from_array_hist ctx r op ne key_values

  def map 'v 't (g: v -> t) (hmap: hashmap v) : hashmap t =
    hashmap.map g hmap

  def map_with_key 'v 't (g: k -> v -> t) (hmap: hashmap v) : hashmap t =
    hashmap.map_with_key g hmap

  def update [u] 'v (key_values: [u](k, v)) (hmap: hashmap v) =
    hashmap.update key_values hmap

  def to_array 'v (hmap: hashmap v) : [](k, v) =
    hashmap.to_array hmap

  def size 'v (hmap: hashmap v) =
    hashmap.size hmap

  def member 'v (ctx: ctx) (k: k) (hmap: hashmap v) : bool =
    hashmap.member ctx k hmap

  def not_member 'v (ctx: ctx) (key: k) (hmap: hashmap v) : bool =
    hashmap.not_member ctx key hmap

  def lookup 'v (ctx: ctx) (k: k) (hmap: hashmap v) : opt v =
    hashmap.lookup ctx k hmap
}
