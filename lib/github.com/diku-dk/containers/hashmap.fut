-- | Static hashmaps.
--
-- Implementations of the `map`@mtype@"map" module type using hash-based data structures.

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
  val from_array_nodup [n] 'v :
    ctx -> [n](key, v) -> ?[f].map ctx [n] [f] v

  -- | Create hashmap with default value. If any keys are duplicates
  -- then the function call will never finish. It does less work than
  -- the safe variant.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_rep_nodup [n] 'v :
    ctx
    -> [n]key
    -> v
    -> ?[f].map ctx [n] [f] v

  -- | Combine key-value pairs into a map using the provided
  -- associative and commutative operation. Keys that are not present
  -- in the map is not added.
  --
  -- **Work:** *O(n + u ✕ W(op))*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val adjust [n] [f] [u] 'v :
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
  val insert_with [n] [f] [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> ?[n'][f'].map ctx [n'] [f'] v
}

-- | This is an implementation of a static hash table using [two level
-- hashing](https://en.wikipedia.org/wiki/Double_hashing). The modules time
-- complexities assumes only unique keys but the modules does work with
-- duplicate keys.
module mk_two_level_hashmap (K: hashkey) (E: rng_engine with int.t = u64)
  : two_level_hashmap
    with key = K.key
    with ctx = K.ctx = {
  module key = K
  module engine = E
  type rng = engine.rng
  type key = key.key
  type~ ctx = key.ctx
  module array = mk_array_key K E

  -- | The hashmap data type.
  -- n: The number of keys.
  -- f: The level two size.
  -- ctx: The context used for the lookup function.
  -- keys: The n unique keys in the hashmap.
  -- values: The values for each corresponding key where each index
  -- matches up with the corresponding key.
  -- offsets: The combination of the level one hash function and level
  -- two hash function will map to an offset in this array into the
  -- keys and values array.
  -- lookup_keys: This array is like keys but the offsets array is not
  -- needed and the hash function can just be used. This array allows
  -- for member to do one less array lookup.
  -- level_one_consts: Constants for the level one hash function.
  -- level_two: The level one hash function will hash into this array
  -- where each array in it contains first an offset into an flattened
  -- f sized array, the size of the subarray is at the second index,
  -- and the remaining vlaues are the constants for the second level
  -- hash function.
  -- rng: The random number generator state.
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
    if n == 0
    then 0
    else -- The offset to get the level two hash function.
         let o = (key.hash ctx level_one_consts k) %% u64.i64 n
         let (arr, cs) = split level_two[i64.u64 o]
         -- The offset into the flat representation
         let o' = i64.u64 arr[0]
         -- The size of the subarray.
         let s = i64.u64 arr[1]
         let y = (key.hash ctx cs k) %% u64.max 1 (u64.i64 s)
         in o' + i64.u64 y

  -- | Given an array of keys with index into an irregular array
  -- (old_keys). And an array of tuples with an index which
  -- corresponds to its original position, the shape of the array, and
  -- the constants that will be used for the hash function for the
  -- subarray. This function will return which subarrays for a given
  -- hash function leads to zero collisions. The remaining will get
  -- new constants and a new shape.
  --
  -- precondition: old_keys must not contain duplicate keys.
  def loop_body [n] [w] [m]
                (ctx: ctx)
                (old_rng: rng)
                (old_keys: [n](key, i64))
                (old_ishape: [w](i64, i64))
                (old_dest: *[m][2 + key.m]u64) : ( rng
                                                 , [](key, i64)
                                                 , [](i64, i64)
                                                 , *[m][2 + key.m]u64
                                                 ) =
    let (new_rng, old_consts) =
      -- Level two hash function constants.
      engine.split_rng w old_rng
      |> map (generate_consts key.m)
      |> unzip
      |> (\(rngs, b) -> (engine.join_rng rngs, b))
    let (_, old_shape) = unzip old_ishape
    let (flat_size, old_shape_offsets) = old_shape |> exscan (+) 0
    let is =
      -- Determine the indices in the flatten array.
      map (\(k, o) ->
             i64.u64 (u64.i64 old_shape_offsets[o] + (key.hash ctx old_consts[o] k %% u64.i64 old_shape[o])))
          old_keys
    let has_no_collisions =
      -- Collisions in the flatten array.
      rep 1
      |> hist (+) 0 flat_size is
      |> map (<= 1)
    let flag_idxs =
      map2 (\i j -> if i == 0 then -1 else j) old_shape old_shape_offsets
    let flags =
      -- Flags for the flatten array.
      scatter (rep false) flag_idxs (rep true)
    let seg_has_no_collision =
      -- A segmented reduce to determine which segments has collisions.
      segmented_reduce (&&) true flags has_no_collisions
      |> sized w
    let new_offsets =
      -- We wanna filter every key that lead to zero collisions. So we
      -- have to update every key that lead to a collision with a new
      -- offset. This is done by mapping every segment which had no
      -- collisions to zero and otherwise to 1.
      map (\f -> if f then 0 else 1) seg_has_no_collision
      |> scan (+) 0
      |> map2 (\f o -> if f then -1 else o - 1) seg_has_no_collision
    let new_keys =
      -- Filter every key which lead to no collisions and update the
      -- offset.
      filter (\(_, o) -> not seg_has_no_collision[o]) old_keys
      |> map (\(key, o) -> (key, new_offsets[o]))
    let (idone, inot_done) =
      -- The subarrays are partitioned by if the subarray lead to zero
      -- collisions and is therefore done.
      (indices old_ishape)
      |> partition (\i -> seg_has_no_collision[i])
    let not_done =
      -- Increment the shape size to get better odds for the second
      -- level hash function of having no collisions.
      map (\i -> old_ishape[i]) inot_done
      |> map (\(i, shp) -> (i, shp + 1))
    let done_consts = map (\i -> old_consts[i]) idone
    let done_shape =
      map (\i -> old_ishape[i]) idone
      |> map (\(_, shp) -> [0, u64.i64 shp])
    let js =
      map (\i -> old_ishape[i]) idone
      |> map (\(i, _) -> i)
    let done =
      map2 (\a b -> a ++ b) done_shape done_consts
    let new_dest = scatter old_dest js done
    in ( new_rng
       , new_keys
       , not_done
       , new_dest
       )

  -- | Construct a level two hash function.
  --
  -- precondition: keys must not contain duplicates.
  def from_array_rep_nodup [n] 'v
                           (ctx: ctx)
                           (keys: [n]key)
                           (ne: v) : ?[f].map ctx [n] [f] v =
    let r = engine.rng_from_seed [123, i32.i64 n]
    -- The level one hash function is determined here.
    let (r, level_one_consts) = generate_consts key.m r
    -- The indices to the subarray the keys will land in.
    let is = map (i64.u64 <-< (%% u64.max 1 (u64.i64 n)) <-< key.hash ctx level_one_consts) keys
    -- The number of keys in a given subarray.
    let level_one_counts = hist (+) 0i64 n is (rep 1)
    let ishape =
      -- The index into the level one array for level two hash functions
      -- and the shape for each subarray.
      zip (iota n) level_one_counts
      |> filter ((!= 0) <-< (.1))
      |> map (\(i, s) -> (i, s ** 2))
    let level_one_offsets =
      -- The initial offsets into the the flatten array for every key.
      map (i64.bool <-< (!= 0)) level_one_counts
      |> scan (+) 0
      |> map2 (\f o -> if f != 0 then o - 1 else -1) level_one_counts
    let init_keys = map2 (\k i -> (k, level_one_offsets[i])) keys is
    let dest = replicate n (replicate (2 + key.m) 0)
    let (final_r, _, _, done) =
      -- Loop until all the hash function have been found that lead
      -- to no collisions.
      loop (old_rng, old_keys, old_not_done, old_done) =
             (r, init_keys, ishape, dest)
      while length old_keys != 0 do
        let ( new_rng
            , new_keys
            , new_not_done
            , new_done
            ) =
          loop_body ctx old_rng old_keys old_not_done old_done
        in ( new_rng
           , new_keys
           , new_not_done
           , new_done
           )
    let t_done = transpose done
    let level_two_shape = map i64.u64 t_done[1]
    let (flat_size, level_two_offsets) =
      -- The offsets into the flatten array.
      level_two_shape
      |> exscan (+) 0
      |> (\(a, b) -> (a, map2 (\f o -> if f == 0 then 0 else o) level_two_shape b))
    let t_done[0] = map u64.i64 level_two_offsets
    let level_two = transpose t_done
    let hash2 =
      level_two_hash ctx
                     level_one_consts
                     level_two
    let js = map hash2 keys
    let temp_offsets = hist i64.min i64.highest flat_size js (iota n)
    let key_reordered =
      -- Reorder the keys so they match the combined hash functions.
      temp_offsets
      |> filter (\o -> 0 <= o && o < n)
      |> map (\o -> keys[o])
      |> sized n
    let offsets =
      -- The offsets into the keys array.
      hist i64.min i64.highest flat_size (map hash2 key_reordered) (indices key_reordered)
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
    in from_array_rep_nodup ctx keys ne

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
    let is =
      -- The smallest indices for each key-value pair.
      hist i64.min i64.highest n js (indices key_values)
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

  def from_array_nodup [n] 'v
                       (ctx: ctx)
                       (key_values: [n](key, v)) : ?[f].(map ctx [n] [f] v) =
    if n == 0
    then empty ctx :> map ctx [n] [0] v
    else let (keys, values) = unzip key_values
         let hmap = from_array_rep_nodup ctx keys values[0]
         in update hmap key_values

  def adjust [n] [f] [u] 'v
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
    in adjust op ne hmap key_values

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
         in if (hmap.ctx, k') key.== (ctx, k) then j else -1

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
         in (hmap.ctx, hmap.lookup_keys[i]) key.== (ctx, k)

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
           -- First remove keys that are in the hashmap then
           -- deduplicate the keys.
           key_values
           |> map (.0)
           |> filter (\k -> not_member ctx k hmap)
           |> array.dedup ctx (hmap.rng)
         let keys' = map (.0) key_values'
         let filler = if u < n then key_values'[0].1 else key_values[0].1
         let keys'' = keys ++ keys'
         let hmap' = from_array_rep_nodup ctx keys'' filler
         in update hmap' (key_values ++ key_values') with rng = rng

  def insert_with [n] [f] [u] 'v
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
    let hmap' = from_array_rep_nodup ctx keys'' ne
    in adjust op ne hmap' (key_values ++ key_values') with rng = rng

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

-- | Create an implementation of `map`@mtype@"map" using two level hash tables.
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

  def adjust [n] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    hashmap.adjust op ne hmap key_values

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n].map [n] v =
    hashmap.from_array_hist ctx op ne key_values

  def from_array_nodup [n] 'v (ctx: ctx) (key_values: [n](key, v)) : map [n] v =
    hashmap.from_array_nodup ctx key_values

  def from_array_rep_nodup [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : map [n] v =
    hashmap.from_array_rep_nodup ctx keys ne

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

  def insert_with [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map [n] v)
                  (key_values: [u](key, v)) =
    hashmap.insert_with ctx op ne hmap key_values
}

module type open_addressing_hashmap = {
  type key

  type ctx

  type map 'ctx [n] [f] 'v

  val member [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> bool

  val not_member [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> bool

  val lookup [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> opt v

  val from_array [u] 'v :
    ctx -> [u](key, v) -> ?[n][f].map ctx [n] [f] v

  val from_array_rep [u] 'v :
    ctx
    -> [u]key
    -> v
    -> ?[n][f].map ctx [n] [f] v

  val from_array_hist [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> [u](key, v)
    -> ?[n][f].map ctx [n] [f] v

  val from_array_nodup [n] 'v :
    ctx -> [n](key, v) -> ?[f].map ctx [n] [f] v

  val from_array_rep_nodup [n] 'v :
    ctx
    -> [n]key
    -> v
    -> ?[f].map ctx [n] [f] v

  val adjust [n] [f] [u] 'v :
    (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> map ctx [n] [f] v

  val map [n] [f] 'v 't :
    (g: v -> t) -> map ctx [n] [f] v -> map ctx [n] [f] t

  val map_with_key [n] [f] 'v 't :
    (g: key -> v -> t)
    -> map ctx [n] [f] v
    -> map ctx [n] [f] t

  val to_array [n] [f] 'v : map ctx [n] [f] v -> [n](key, v)

  val update [n] [f] [u] 'v :
    map ctx [n] [f] v
    -> [u](key, v)
    -> map ctx [n] [f] v

  val size [n] [f] 'v : map ctx [n] [f] v -> i64

  val context [n] [f] 'v : map ctx [n] [f] v -> ctx

  val insert [n] [f] [u] 'v :
    ctx
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> ?[n'][f'].map ctx [n'] [f'] v

  val insert_with [n] [f] [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> ?[n'][f'].map ctx [n'] [f'] v
}

module mk_open_addressing_hashmap
  (K: hashkey)
  (E: rng_engine with int.t = u64)
  (P: {
    val hashmap_size : i64 -> i64
    val probe : u64 -> u64
  })
  : open_addressing_hashmap
    with key = K.key
    with ctx = K.ctx = {
  module key = K
  module engine = E
  type rng = engine.rng
  type key = key.key
  type~ ctx = key.ctx

  local def probe = P.probe
  local def hashmap_size = P.hashmap_size

  local
  #[inline]
  def generate_consts (n: i64) (r: rng) =
    #[sequential]
    engine.split_rng n r
    |> map engine.rand
    |> unzip
    |> (\(a, b) -> (engine.join_rng a, b))

  type map 'ctx [n] [f] 'v =
    { ctx: ctx
    , consts: [key.m]u64
    , keys: [n]key
    , values: [n]v
    , lookup_keys: [f](bool, key)
    , offsets: [f]i64
    , max_iters: u64
    , rng: rng
    }

  local
  def empty 'v ctx : ?[f].map ctx [0] [f] v =
    { ctx
    , consts = rep 0
    , keys = []
    , values = []
    , offsets = []
    , lookup_keys = []
    , max_iters = 0
    , rng = engine.rng_from_seed [123]
    }

  def from_array_rep [u] 'v
                     (ctx: ctx)
                     (keys: [u]key)
                     (ne: v) : ?[n][f].map ctx [n] [f] v =
    let r = engine.rng_from_seed [123, i32.i64 u]
    let (r, consts) = generate_consts K.m r
    let filler = (false, keys[0])
    let keq a b = (ctx, a) K.== (ctx, b)
    let hash = K.hash ctx consts
    let hashes' = map hash keys
    let size = P.hashmap_size u
    let (max_iters, lookup_keys, _) =
      loop (i, old_dest, old_keys) =
             (0u64, replicate size filler, zip hashes' keys)
      while length old_keys != 0 do
        let hashes =
          map (i64.u64
               <-< (%% u64.i64 size)
               <-< (+ probe i)
               <-< (.0))
              old_keys
        let (is, vs) =
          hist i64.min i64.highest size hashes (indices (old_keys))
          |> map (\i ->
                    if 0 <= i && i < size && !old_dest[hashes[i]].0
                    then (hashes[i], (true, old_keys[i].1))
                    else (-1, filler))
          |> unzip
        let new_dest = scatter (copy old_dest) is vs
        let new_keys =
          zip hashes old_keys
          |> filter (\(i, (_, k)) ->
                       new_dest[i].0
                       && !(k `keq` new_dest[i].1))
          |> map (.1)
        in (i + 1, new_dest, new_keys)
    let keys = filter (.0) lookup_keys |> map (.1)
    let offsets =
      map (i64.bool <-< (.0)) lookup_keys
      |> scan (+) 0
      |> map (+ (-1))
    let s = length keys
    let keys = sized s keys
    in { rng = r
       , consts = consts
       , keys = keys
       , values = replicate s ne
       , max_iters = max_iters + 1
       , lookup_keys = lookup_keys
       , offsets = offsets
       , ctx = ctx
       }

  def from_array_rep_nodup [n] 'v
                           (ctx: ctx)
                           (keys: [n]key)
                           (ne: v) : ?[f].map ctx [n] [f] v =
    let hmap = from_array_rep ctx keys ne
    in hmap with keys = sized n hmap.keys
            with values = sized n hmap.values

  local
  def lookup_idx [n] [f] 'v
                 (ctx: ctx)
                 (k: key)
                 (hmap: map ctx [n] [f] v) : i64 =
    let h = K.hash ctx hmap.consts k
    let keq a b = (hmap.ctx, a) K.== (ctx, b)
    in (.2)
       <| loop (is_found, i, j) = (false, 0, -1)
          while i < hmap.max_iters || is_found do
            let idx = i64.u64 ((h + probe i) %% u64.i64 f)
            let (t, k') = hmap.lookup_keys[idx]
            let is_valid = t && (k' `keq` k)
            in ( is_valid
               , i + 1
               , if is_valid then idx else j
               )

  local
  def offset [n] [f] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] v) : i64 =
    let i = lookup_idx ctx k hmap
    in if i == -1
       then -1
       else hmap.offsets[i]

  def member [n] [f] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] v) : bool =
    -1 != lookup_idx ctx k hmap

  def not_member [n] [f] 'v
                 (ctx: ctx)
                 (k: key)
                 (hmap: map ctx [n] [f] v) : bool =
    not (member ctx k hmap)

  def lookup [n] [f] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] v) : opt v =
    let i = offset ctx k hmap
    in if i == -1
       then #none
       else some hmap.values[i]

  def update [n] [f] [u] 'v
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let js = map (\k -> offset hmap.ctx k hmap) keys
    let is =
      -- The smallest indices for each key-value pair.
      hist i64.min i64.highest n js (indices key_values)
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

  def from_array_nodup [n] 'v
                       (ctx: ctx)
                       (key_values: [n](key, v)) : ?[f].map ctx [n] [f] v =
    let hmap = from_array ctx key_values
    in hmap with keys = sized n hmap.keys
            with values = sized n hmap.values

  def adjust [n] [f] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let is = map (\k -> offset hmap.ctx k hmap) keys
    let vs = reduce_by_index (copy hmap.values) op ne is values
    in hmap with values = vs

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
    let keys = map (.0) key_values
    let hmap = from_array_rep ctx keys ne
    in adjust op ne hmap key_values

  def to_array [n] [f] 'v (hmap: map ctx [n] [f] v) : [](key, v) =
    zip hmap.keys hmap.values

  def size [n] [f] 'v (hmap: map ctx [n] [f] v) =
    length hmap.keys

  def context [n] [f] 'v (hmap: map ctx [n] [f] v) =
    hmap.ctx

  def insert [n] [f] [u] 'v
             (ctx: ctx)
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    if n == 0 && u == 0
    then hmap
    else let key_values' = to_array hmap
         let keys =
           -- First remove keys that are in the hashmap then
           -- deduplicate the keys.
           key_values
           |> map (.0)
           |> filter (\k -> not_member ctx k hmap)
         let keys' = map (.0) key_values'
         let filler = if u < n then key_values'[0].1 else key_values[0].1
         let keys'' = keys ++ keys'
         let hmap' = from_array_rep ctx keys'' filler
         in update hmap' (key_values ++ key_values')

  def insert_with [n] [f] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map ctx [n] [f] v)
                  (key_values: [u](key, v)) =
    let key_values' = to_array hmap
    let keys =
      key_values
      |> map (.0)
      |> filter (\k -> not_member ctx k hmap)
    let keys' = map (.0) key_values'
    let keys'' = keys ++ keys'
    let hmap' = from_array_rep ctx keys'' ne
    in adjust op ne hmap' (key_values ++ key_values')

  def map_with_key [n] [f] 'v 't
                   (g: key -> v -> t)
                   (hmap: map ctx [n] [f] v) : map ctx [n] [f] t =
    let vs = map2 g hmap.keys hmap.values
    in { ctx = hmap.ctx
       , keys = hmap.keys
       , lookup_keys = hmap.lookup_keys
       , values = vs
       , max_iters = hmap.max_iters
       , offsets = hmap.offsets
       , consts = hmap.consts
       , rng = hmap.rng
       }

  def map [n] [f] 'v 't
          (g: v -> t)
          (hmap: map ctx [n] [f] v) : map ctx [n] [f] t =
    map_with_key (\_ v -> g v) hmap
}

module mk_linear_hashmap (K: hashkey) (E: rng_engine with int.t = u64)
  : map
    with key = K.key
    with ctx = K.ctx = {
  module hashmap = mk_open_addressing_hashmap K E {
    def hashmap_size = (4i64 *)
    def probe = id
  }

  type key = hashmap.key
  type ctx = hashmap.ctx

  type~ map [n] 'v =
    ?[f].hashmap.map ctx [n] [f] v

  def from_array [n] 'v (ctx: ctx) (key_values: [n](key, v)) : ?[m].map [m] v =
    hashmap.from_array ctx key_values

  def from_array_rep [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : ?[m].map [m] v =
    hashmap.from_array_rep ctx keys ne

  def adjust [n] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    hashmap.adjust op ne hmap key_values

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n].map [n] v =
    hashmap.from_array_hist ctx op ne key_values

  def from_array_nodup [n] 'v (ctx: ctx) (key_values: [n](key, v)) : map [n] v =
    hashmap.from_array_nodup ctx key_values

  def from_array_rep_nodup [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : map [n] v =
    hashmap.from_array_rep_nodup ctx keys ne

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

  def insert_with [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map [n] v)
                  (key_values: [u](key, v)) =
    hashmap.insert_with ctx op ne hmap key_values
}
