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
  -- neutral element `ne`@term must be given. To perform this
  -- reduction a definition of key equality must be given `eq`@term,
  -- this is done as to figure out which elements will be reduced.
  val reduce_by_key [n] 'v : ctx -> rng -> (v -> v -> v) -> v -> [n](k, v) -> ?[m].(rng, [m](k, v))

  -- | Removes duplicate elements from an array as defined by an
  -- equality `eq`@term. This implementation works much like
  -- `reduce_by_key`@term but saves some steps which makes it faster.
  val dedup [n] : ctx -> rng -> [n]k -> ?[m].(rng, [m]k)
}

module array (K: key) (E: rng_engine with int.t = K.i)
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

  def reduce_by_key [n] 'v
                    (ctx: ctx)
                    (r: rng)
                    (op: v -> v -> v)
                    (ne: v)
                    (arr: [n](k, v)) : (rng, [](k, v)) =
    let keq = key.eq ctx
    let (r, est) = map (.0) arr |> estimate_distinct ctx r 128
    let (reduction, _, _, final_rng) =
      -- Expected number of iterations is O(log n).
      loop (reduced, not_reduced, size, old_rng) = ([], arr, est, r)
      while length not_reduced != 0 do
        let (new_rng, consts) = generate_consts key.m old_rng
        let keys = map (.0) not_reduced
        let alloc_size = size + size / 2
        let h = (% alloc_size) <-< key.hash ctx consts
        let hashes = map h keys
        let collision_idxs =
          -- Find the smallest indices in regards to each hash to resolve collisions.
          hist i64.min i64.highest alloc_size hashes (indices keys)
        let (new_reduced, new_not_reduced) =
          -- Elements with the same hash as the element at the smallest index will be reduced.
          partition (\(h', elem) ->
                       not_reduced[collision_idxs[h']].0
                       `keq` elem.0)
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
        in (reduced ++ new_reduced, new_not_reduced, new_size, new_rng)
    in (final_rng, reduction)

  def dedup [n] (ctx: ctx) (r: rng) (arr: [n]k) : ?[m].(rng, [m]k) =
    let keq = key.eq ctx
    let (r, est) = estimate_distinct ctx r 128 arr
    let (uniques, _, _, final_rng) =
      loop (uniques, elems, size, old_rng) = ([], arr, est, r)
      while length elems != 0 do
        let (new_rng, consts) = generate_consts key.m old_rng
        let alloc_size = size + size / 2
        let h = (% alloc_size) <-< key.hash ctx consts
        let hashes = map h elems
        let collision_idxs =
          hist i64.min i64.highest alloc_size hashes (indices elems)
        let new_uniques =
          filter (!= i64.highest) collision_idxs
          |> map (\i -> elems[i])
        let new_size = i64.max 1024 (size - length new_uniques)
        let new_elems =
          elems
          |> zip hashes
          |> filter (\(h', v) ->
                       not (elems[collision_idxs[h']] `keq` v))
          |> map (.1)
        in (uniques ++ new_uniques, new_elems, new_size, new_rng)
    in (final_rng, uniques)
}
