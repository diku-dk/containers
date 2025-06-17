-- | Functions on pure arrays.
--
-- This module contains modules and functions for manipulating arrays. Which
-- might be something like deduplication or a generalized reduction by a key.

import "../cpprandom/random"
import "../sorts/radix_sort"
import "../segmented/segmented"
import "hashkey"
import "opt"

local
-- | Fairly simple functions on arrays. Implemented by the module `array`@term.
module type array = {
  -- | True if the provided arrays have the same size and the same elements as
  -- determined by the provided operator.
  val eq [n] [m] 't : (eq: t -> t -> bool) -> [n]t -> [m]t -> bool

  -- | Lexicographical less-or-equal comparison of two arrays.
  val le [n] [m] 't : (lte: t -> t -> bool) -> [n]t -> [m]t -> bool
}

module array : array = {
  -- | Compare two arrays for equality using a provided operator.
  def eq [n] [m] 't ((==): t -> t -> bool) (x: [n]t) (y: [m]t) =
    n i64.== m
    && (loop (ok, i) = (true, 0)
        while ok && i < n do
          (ok && (x[i] == y[i]), i + 1)).0

  -- Lexicographical comparison of two arrays. Returns true if `a <= b`.
  def le [n] [m] 't ((<=): t -> t -> bool) (a: [n]t) (b: [m]t) : bool =
    let minlen = i64.min n m
    let (<) = \x y -> x <= y && !(y <= x)
    let cmp =
      map2 (\x y : opt bool ->
              if x < y
              then #some true
              else if y < x
              then #some false
              else #none)
           (take minlen a)
           (take minlen b)
    in match first_some cmp
       case #some res -> res
       case #none -> n i64.<= m
}

-- | A module type for functions on arrays of keys. Use `mk_array_key`@term
-- to produce a module that implements this module type, given a random number
-- generator.
module type array_key = {
  -- | The key type.
  type key

  -- | The context type.
  type ctx

  -- | The random number generator.
  type rng

  -- | Reduce by key works like Futharks `reduce_by_index` but instead of the
  -- need to make every value correspond to an index in some array it can
  -- instead correspond to a key. Here an array of `n` key `k` and value `v`
  -- pairs are given as an array. And every value with the same key will be
  -- reduced with an associative and commutative operator `op`, futhermore an
  -- neutral element `ne` must be given.
  val reduce_by_key [n] 'v :
    ctx
    -> rng
    -> (v -> v -> v)
    -> v
    -> [n](key, v)
    -> ?[m].(rng, [m](key, v))

  -- | Removes duplicate elements from an array as defined by the equality
  -- relation of the key. This implementation works much like `reduce_by_key`
  -- but saves some steps which makes it faster.
  val dedup [n] : ctx -> rng -> [n]key -> ?[m].(rng, [m]key)
}

module mk_array_key_params
  (I: integral)
  (U: integral)
  (K: hashkey with hash = U.t)
  (E: rng_engine with t = K.const)
  : array_key
    with rng = E.rng
    with key = K.key
    with ctx = K.ctx = {
  module key = K
  module engine = E
  type int = I.t
  type uint = U.t
  type rng = engine.rng
  type key = key.key
  type~ ctx = K.ctx

  def to_int : uint -> int =
    I.i64 <-< U.to_i64

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
                        (factor: uint)
                        (keys: [n]key) : (rng, uint) =
    if n == 0
    then (rng, U.i64 0)
    else let sample_size = U.(max (i64 1) (i64 n / factor))
         let (rng, consts) = generate_consts key.c rng
         let sample = keys[:U.to_i64 sample_size]
         let hs =
           map ((U.%% sample_size) <-< key.hash ctx consts) sample
           |> radix_sort (U.num_bits - U.clz sample_size) U.get_bit
         let est =
           iota (U.to_i64 sample_size)
           |> map U.i64
           |> map (\i -> U.bool (U.(i == i64 0 || (hs[to_i64 (i - i64 1)] != hs[to_i64 i]))))
           |> U.sum
           |> (U.* factor)
           |> (U.i64 2 U.*)
           |> (U.i64 1 U.+)
         let size = U.(max (i64 1024) est)
         in (rng, U.(min (i64 n) size))

  def dedup [n] (ctx: ctx) (r: rng) (arr: [n]key) : ?[m].(rng, [m]key) =
    if n == 0
    then (r, [])
    else let keq a b = (ctx, a) key.== (ctx, b)
         let dest = replicate n arr[0]
         let (r, est) = estimate_distinct ctx r (U.i64 128) arr
         let (uniques, _, _, final_size, final_rng) =
           loop (uniques, elems, size, old_size, old_rng) = (dest, copy arr, copy est, 0, r)
           while length elems != 0 do
             let (new_rng, consts) = generate_consts key.c old_rng
             let alloc_size = U.(size + size / i64 2)
             let h = to_int <-< (U.%% alloc_size) <-< key.hash ctx consts
             let hashes = map h elems
             let collision_idxs =
               hist I.min
                    I.highest
                    (U.to_i64 alloc_size)
                    (map I.to_i64 hashes)
                    (map I.i64 (indices hashes))
             let new_uniques =
               filter (I.!= I.highest) collision_idxs
               |> map (\i -> #[unsafe] elems[I.to_i64 i])
             let new_size = U.(max (i64 1024) (size - i64 (length new_uniques)))
             let new_elems =
               elems
               |> zip hashes
               |> filter (\(h', v) ->
                            #[unsafe]
                            I.(collision_idxs[to_i64 h'] == highest)
                            || not I.(elems[to_i64 collision_idxs[to_i64 h']] `keq` v))
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
                    (arr: [n](key, v)) : (rng, [](key, v)) =
    if n == 0
    then (r, [])
    else let keq a b = (ctx, a) key.== (ctx, b)
         let dest = replicate n arr[0]
         let (r, est) = map (.0) arr |> estimate_distinct ctx r (U.i64 128)
         let (reduction, _, _, final_size, final_rng) =
           -- Expected number of iterations is O(log n).
           loop (reduced, not_reduced, size, old_size, old_rng) = (dest, arr, copy est, 0, r)
           while length not_reduced != 0 do
             let (new_rng, consts) = generate_consts key.c old_rng
             let keys = map (.0) not_reduced
             let alloc_size = U.(size + size / i64 2)
             let h = to_int <-< (U.%% alloc_size) <-< key.hash ctx consts
             let hashes = map h keys
             let collision_idxs =
               -- Find the smallest indices in regards to each hash to resolve collisions.
               hist I.min
                    I.highest
                    (U.to_i64 alloc_size)
                    (map I.to_i64 hashes)
                    (map I.i64 (indices hashes))
             let (new_reduced, new_not_reduced) =
               -- Elements with the same hash as the element at the smallest index will be reduced.
               partition (\(h', elem) ->
                            I.(collision_idxs[to_i64 h'] != highest)
                            && I.(not_reduced[to_i64 collision_idxs[to_i64 h']].0 `keq` elem.0))
                         (zip hashes not_reduced)
             let new_size = U.(max (i64 1024) (size - i64 (length new_reduced)))
             let (hashes, elems) = unzip new_reduced
             let values = map (.1) elems
             let new_reduced =
               -- Reduce all elements which had the same key and was resolved.
               hist op ne (U.to_i64 alloc_size) (map I.to_i64 hashes) values
               |> zip collision_idxs
               |> filter ((I.!= I.highest) <-< (.0))
               |> map (\(i, v) -> (not_reduced[I.to_i64 i].0, v))
             let new_not_reduced = unzip new_not_reduced |> (.1)
             let is = map (+ old_size) (indices new_reduced)
             in ( scatter reduced is new_reduced
                , new_not_reduced
                , new_size
                , old_size + length new_reduced
                , new_rng
                )
         in (final_rng, take final_size reduction)
}

module mk_array_key = mk_array_key_params i64 u64
