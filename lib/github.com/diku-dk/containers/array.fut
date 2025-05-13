-- | Array related operations, like `reduce_by_key`@term.
--
-- This module contains functions for manipulating arrays.
-- Which might be something like deduplication or a generalized
-- reduction by a key.

import "../cpprandom/random"
import "../sorts/radix_sort"
import "key"

module type array = {
  -- | The key type.
  type k

  -- | The context type.
  type ctx

  -- | The random number generator.
  type rng

  -- | Reduce by key works like Futharks `reduce_by_index` but instead
  -- of the need to make every value correspond to an index in some
  -- array it can instead correspond to a key. Here an array of
  -- `n`@term key `k`@term and value `v`@term pairs are given as an
  -- array. And every value with the same key will be reduced with an
  -- associative and commutative operator `op`@term, futhermore an
  -- neutral element `ne`@term must be given.
  val reduce_by_key [n] 'v : ctx -> rng -> (v -> v -> v) -> v -> [n](k, v) -> ?[m].(rng, [m](k, v))

  -- | Removes duplicate elements from an array as defined by an
  -- equality `eq`@term. This implementation works much like
  -- `reduce_by_key`@term but saves some steps which makes it faster.
  val dedup [n] : ctx -> rng -> [n]k -> ?[m].(rng, [m]k)
}

module mk_array (K: key) (E: rng_engine with int.t = K.i)
  : array
    with rng = E.rng
    with k = K.k
    with ctx = K.ctx = {
  module key = K
  module engine = E
  type rng = engine.rng
  type int = key.i
  type k = key.k
  type~ ctx = K.ctx
  module int = engine.int

  local
  #[inline]
  def generate_consts (n: i64) (r: rng) =
    #[sequential]
    engine.split_rng n r
    |> map engine.rand
    |> unzip
    |> (\(a, b) -> (engine.join_rng a, b))

  local
  def estimate_distinct [n]
                        (ctx: ctx)
                        (rng: rng)
                        (factor: i64)
                        (keys: [n]k) : (rng, i64) =
    if n == 0
    then (rng, 0)
    else let sample_size = i64.max 1 (n / factor)
         let (rng, consts) = generate_consts key.m rng
         let (rng, i) = engine.rand rng
         let sample = (rotate (int.to_i64 i % n) keys)[:sample_size]
         let is =
           map ((% sample_size) <-< key.hash ctx consts) sample
           |> radix_sort (i64.num_bits - i64.clz sample_size) i64.get_bit
         let est =
           tabulate (n / factor) (\i -> i64.bool (i == 0 || is[i - 1] != is[i]))
           |> i64.sum
           |> (* factor)
           |> (2 *)
           |> (1 +)
         let size = i64.max 1024 est
         in (rng, i64.min n size)

  def dedup [n] (ctx: ctx) (r: rng) (arr: [n]k) : ?[m].(rng, [m]k) =
    if n == 0
    then (r, [])
    else let keq = key.eq ctx
         let dest = replicate n arr[0]
         let (r, est) = estimate_distinct ctx r 128 arr
         let (uniques, _, _, final_size, final_rng) =
           loop (uniques, elems, size, old_size, old_rng) = (dest, copy arr, est, 0, r)
           while length elems != 0 do
             let (new_rng, consts) = generate_consts key.m old_rng
             let bounded_size = i64.min size 10000000
             let alloc_size = bounded_size + bounded_size / 2
             let bounded_length = i64.min bounded_size (length elems)
             let h = (% alloc_size) <-< key.hash ctx consts
             let hashes = map h elems
             let collision_idxs =
               hist i64.min
                    i64.highest
                    alloc_size
                    hashes[:bounded_length]
                    (iota bounded_length)
             let new_uniques =
               filter (!= i64.highest) collision_idxs
               |> map (\i -> elems[i])
             let new_size = i64.max 1024 (size - length new_uniques)
             let new_elems =
               elems
               |> zip hashes
               |> filter (\(h', v) ->
                            collision_idxs[h'] == i64.highest
                            || not (elems[collision_idxs[h']] `keq` v))
               |> map (.1)
             let is = map (+ old_size) (indices new_uniques)
             in ( scatter uniques is new_uniques
                , new_elems
                , new_size
                , old_size + length new_uniques
                , new_rng
                )
         in (final_rng, take final_size uniques)

  def reduce_by_key [n] 'v
                    (ctx: ctx)
                    (r: rng)
                    (op: v -> v -> v)
                    (ne: v)
                    (arr: [n](k, v)) : (rng, [](k, v)) =
    if n == 0
    then (r, [])
    else let keq = key.eq ctx
         let dest = replicate n arr[0]
         let (r, est) = map (.0) arr |> estimate_distinct ctx r 128
         let (reduction, _, _, final_size, final_rng) =
           -- Expected number of iterations is O(log n).
           loop (reduced, not_reduced, size, old_size, old_rng) = (dest, arr, est, 0, r)
           while length not_reduced != 0 do
             let (new_rng, consts) = generate_consts key.m old_rng
             let keys = map (.0) not_reduced
             let bounded_size = i64.min size 5000000
             let alloc_size = bounded_size + bounded_size / 2
             let bounded_length = i64.min bounded_size (length not_reduced)
             let h = (% alloc_size) <-< key.hash ctx consts
             let hashes = map h keys
             let collision_idxs =
               -- Find the smallest indices in regards to each hash to resolve collisions.
               hist i64.min
                    i64.highest
                    alloc_size
                    hashes[:bounded_length]
                    (iota bounded_length)
             let (new_reduced, new_not_reduced) =
               -- Elements with the same hash as the element at the smallest index will be reduced.
               partition (\(h', elem) ->
                            collision_idxs[h'] != i64.highest
                            && (not_reduced[collision_idxs[h']].0 `keq` elem.0))
                         (zip hashes not_reduced)
             let new_size = i64.max 1024 (size - length new_reduced)
             let (hashes, elems) = unzip new_reduced
             let values = map (.1) elems
             let new_reduced =
               -- Reduce all elements which had the same key and was resolved.
               hist op ne alloc_size hashes values
               |> zip collision_idxs
               |> filter ((!= i64.highest) <-< (.0))
               |> map (\(i, v) -> (not_reduced[i].0, v))
             let new_not_reduced = unzip new_not_reduced |> (.1)
             let is = map (+ old_size) (indices new_reduced)
             in ( scatter reduced is new_reduced
                , new_not_reduced
                , new_size
                , old_size + length new_reduced
                , new_rng
                )
         in (final_rng, take final_size reduction)

  local
  def reduce_by_key' [n] [m] 'v
                     (ctx: ctx)
                     (r: rng)
                     (op: v -> v -> v)
                     (ne: v)
                     (keys: [m]k)
                     (arr: [n](k, v)) : (rng, [](k, v), [](k, v)) =
    if n == 0 || m == 0
    then (r, [], arr)
    else let keq = key.eq ctx
         let dest = replicate n arr[0]
         let (reduction, not_done, _, final_size, final_rng) =
           -- Expected number of iterations is O(log n).
           loop (reduced, not_reduced, old_keys, old_size, old_rng) = (dest, arr, keys, 0, r)
           while length old_keys != 0 do
             let (new_rng, consts) = generate_consts key.m old_rng
             let keys = map (.0) not_reduced
             let size = length old_keys
             let alloc_size = size + size / 2
             let h = (% alloc_size) <-< key.hash ctx consts
             let hashes = map h keys
             let collision_idxs =
               -- Find the smallest indices in regards to each hash to resolve collisions.
               hist i64.min i64.highest alloc_size hashes (indices keys)
             let new_not_reduce_keys =
               filter (\(h', k) ->
                         collision_idxs[h'] == i64.highest
                         || not (keys[collision_idxs[h']] `keq` k))
                      (zip hashes keys)
             let (new_reduced, new_not_reduced) =
               -- Elements with the same hash as the element at the smallest index will be reduced.
               partition (\(h', elem) ->
                            collision_idxs[h'] != i64.highest
                            && (not_reduced[collision_idxs[h']].0 `keq` elem.0))
                         (zip hashes not_reduced)
             let (hashes, elems) = unzip new_reduced
             let values = map (.1) elems
             let new_reduced =
               -- Reduce all elements which had the same key and was resolved.
               hist op ne alloc_size hashes values
               |> zip collision_idxs
               |> filter ((!= i64.highest) <-< (.0))
               |> map (\(i, v) -> (not_reduced[i].0, v))
             let new_not_reduced = unzip new_not_reduced |> (.1)
             let is = map (+ old_size) (indices new_reduced)
             in ( scatter reduced is new_reduced
                , new_not_reduced
                , map (.1) new_not_reduce_keys
                , old_size + length new_reduced
                , new_rng
                )
         in (final_rng, take final_size reduction, not_done)

  local
  def blocked_reduce_by_key [n] 'v
                            (ctx: ctx)
                            (r: rng)
                            (op: v -> v -> v)
                            (ne: v)
                            (arr: [n](k, v)) : (rng, [](k, v)) =
    if n == 0
    then (r, [])
    else let block = 10000000
         let (r, keys) = dedup ctx r (map (.0) arr)
         let m = length keys
         let keys = keys :> [m]k
         let n_blocks = m / block
         let q_rest = m % block
         let keys = sized (n_blocks * block + q_rest) keys
         let (blocks, rest) = split keys
         let dest = replicate n arr[0]
         let (r, reduced, not_reduced, size) =
           loop (old_rng, old_reduced, old_not_reduced, old_size) =
                  (r, dest, arr, 0)
           for block in unflatten blocks do
             let (new_rng, new_reduced, new_not_reduced) =
               reduce_by_key' ctx old_rng op ne block old_not_reduced
             let is = map (+ old_size) (indices new_reduced)
             in ( new_rng
                , scatter old_reduced is new_reduced
                , new_not_reduced
                , old_size + length new_reduced
                )
         let (r, new_reduced, _) =
           reduce_by_key' ctx r op ne rest not_reduced
         let is = map (+ size) (indices new_reduced)
         let final_reduced = scatter reduced is new_reduced
         in (r, take (size + length new_reduced) final_reduced)
}
