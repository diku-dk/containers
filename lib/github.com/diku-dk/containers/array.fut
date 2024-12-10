-- | Array related operations, like `reduce_by_key`@term.
--
-- This module contains methods of manipulating arrays.
-- Which might be something like deduplication or a generalized
-- reduction by a key.

import "../cpprandom/random"
import "../segmented/segmented"
import "../sorts/radix_sort"
import "../sorts/merge_sort"

module engine = xorshift128plus
type rng = xorshift128plus.rng
                            
local def estimate_distinct' [n] 't
                             (r: rng)
                             (hash: i64 -> t -> i64)
                             (arr: [n]t) =
  let (new_rng, seed) = engine.rand r
  let hashes =
    map (hash (i64.u64 seed)) arr
    |> merge_sort (<=)
  let count =
    tabulate n (\i -> i64.bool (i == 0 || hashes[i - 1] != hashes[i]))
    |> i64.sum
  -- Multiply by two for a better estimate in cases of collisions.
  -- Since roughly half of the keys are expected to result in a collision.
  in (new_rng, 2 * count)
     
local def create_sample [n] 't
                        (arr: [n]t)
                        (rng: rng)
                        (sample_size: i64):
                        (rng, [sample_size]t) =
  let rngs = rng |> engine.split_rng sample_size
  let (new_rngs, idxs) =
    map (
      \r ->
        let (nr, a) = engine.rand r
        in (nr, i64.u64 (a % u64.i64 n))
    ) rngs
    |> unzip
  let new_rng = engine.join_rng new_rngs
  let sample = map (\i -> arr[i]) idxs
  in (new_rng, sample)

local def estimate_distinct [n] 't
                            (rng: rng)
                            (sample_size: i64)
                            (times: i64)
                            (hash: i64 -> t -> i64)
                            (arr: [n]t):
                            (rng, i64) =
  let (new_rng, count) =
    loop (rng, acc) = (rng, 0) for _i < times do 
    let (rng, sample) = create_sample arr rng sample_size
    let (rng, c) = estimate_distinct' rng hash sample
    in (rng, acc + c / times)
  let factor = n / sample_size
  let estimate = i64.min n (factor * i64.max 1 count)
  in (new_rng, estimate)

-- | `reduce_by_key`@term, but paramatized. Here an initial `seed`@term
-- for a random number generater needed. And a `sample_fraction`@term
-- will be needed to determine the amount that will be sampled in each
-- iteration. This is used to estimate the number of distinct
-- elements. A `sample_fraction`@term of 2 will sample half of the pairs in that iteration
-- (*n / 2*) while a value of *x* will sample *x* of the pairs (*n / x*).
-- The `times`@term parameter will determine how many times the estimate
-- will be computed to find an average of the estimates.
def reduce_by_key_param [n] 'k 'v
                        (seed: i32)
                        (sample_fraction: i64)
                        (times: i64)
                        (hash: i64 -> k -> i64)
                        (eq: k -> k -> bool)
                        (ne: v)
                        (op: v -> v -> v)
                        (arr: [n](k, v)): [](k, v) = 
  let r = engine.rng_from_seed [seed]
  let (reduction, _, _) =
    -- Expected number of iterations is O(log n).
    loop (reduced, not_reduced, old_rng) = ([], arr, r) while length not_reduced != 0 do
    let (new_rng, seed) = engine.rand old_rng
    let keys = map (.0) not_reduced
    let sample_size = i64.max 1 (length not_reduced / sample_fraction)
    let (new_rng, size) =
      estimate_distinct new_rng sample_size times hash keys
    let size = i64.max 1024 (size + size / 2)
    let h = (% size) <-< hash (i64.u64 seed)
    let hashes = map h keys
    let collision_idxs =
      -- Find the smallest indices in regards to each hash to resolve collisions.
      hist i64.min i64.highest size hashes (indices hashes)
    let (new_reduced, new_not_reduced) =
      -- Elements with the same hash as the element at the smallest index will be reduced.
      partition (
        \(h', elem) -> not_reduced[collision_idxs[h']].0 `eq` elem.0
      ) (zip hashes not_reduced)
    let (hashes, elems) = unzip new_reduced
    let values = map (.1) elems
    let new_reduced =
      -- Reduce all elements which had the same key and was resolved.
      hist op ne size hashes values
      |> zip collision_idxs
      |> filter ((!=i64.highest) <-< (.0))
      |> map (\(i, v) -> (not_reduced[i].0, v))
    let new_not_reduced = unzip new_not_reduced |> (.1)
    in (reduced ++ new_reduced, new_not_reduced, new_rng)
  in reduction

-- | `dedup`@term, but paramatized. The parameters are the same as in
-- `reduce_by_key`@term.
def dedup_param [n] 't
                (seed: i32)
                (sample_fraction: i64)
                (times: i64)
                (hash: i64 -> t -> i64)
                (eq: t -> t -> bool)
                (arr: [n]t): []t = 
  let r = engine.rng_from_seed [seed]
  let (uniques, _, _) =
    loop (uniques, elems, old_rng) = ([], arr, r) while length elems != 0 do
    let (new_rng, seed) = engine.rand old_rng
    let sample_size = i64.max 1 (length elems / sample_fraction)
    let (new_rng, size) =
      estimate_distinct new_rng sample_size times hash elems
    let size = i64.max 1024 (size + size / 2)
    let h = (% size) <-< hash (i64.u64 seed)
    let hashes = map h elems
    let collision_idxs =
      hist i64.min i64.highest size hashes (indices hashes)
    let new_uniques =
      filter (!= i64.highest) collision_idxs
      |> map (\i -> arr[i])
    let new_elems =
      elems
      |> zip hashes
      |> filter (
        \(h', v) -> not (elems[collision_idxs[h']] `eq` v)
      )
      |> map (.1)
    in (uniques ++ new_uniques, new_elems, new_rng)
  in uniques

--| A hash function that seems to work great for i64 in regards to
-- removing duplicates or reduce by key.
-- The hash function was found [here](http://stackoverflow.com/a/12996028).
def hash_i64 (a: i64) (x: i64): i64 =
  let x = a * x
  let x = (x ^ (x >> 30)) * (i64.u64 0xbf58476d1ce4e5b9)
  let x = (x ^ (x >> 27)) * (i64.u64 0x94d049bb133111eb)
  let y = (x ^ (x >> 31))
  in y

-- | Reduce by key works like Futharks `reduce_by_index` but
-- instead of the need to make every value correspond to an index in
-- some array it can instead correspond to a key. Here an array of `n`@term
-- key `k`@term and value `v`@term pairs are given as an array. And
-- every value with the same key will be reduced with an associative
-- and commutative operator `op`@term, futhermore an neutral element `ne`@term
-- must be given. To perform this reduction a definition of key
-- equality must be given `eq`@term, this is done as to figure out which
-- elements will be reduced. And a good hash function `hash`@term must be
-- chosen, see `hash_i64`@term an for example. Given
-- a hash function with an uniform distribution then this function has the
-- expected work of *O(n * W(op))* where *W(op)* is the work of `op`.
-- The span of this operation is unknown since the span of
-- `reduce_by_index` is not given in the documentation. It generally
-- performs much better for inputs with duplicate keys. The output
-- is most likely unordered.
def reduce_by_key [n] 'k 'v
                  (hash: i64 -> k -> i64)
                  (eq: k -> k -> bool)
                  (ne: v)
                  (op: v -> v -> v)
                  (arr: [n](k, v)): [](k, v) =
  reduce_by_key_param 1 1024 5 hash eq ne op arr

-- | Removes duplicate elements from an array as defined by an
-- equality `eq`@term. This implementation works much like
-- `reduce_by_key`@term but saves some steps which makes it a bit
-- faster. Therefore, it also needs a hash function `hash` with a uniform
-- distribution. Its work is *O(n)* and its span is unknown for the
-- same reason as `reduce_by_key`@term. It also performances about as
-- well as `reduce_by_key`@term. The output is most likely unordered.
def dedup [n] 't
          (hash: i64 -> t -> i64)
          (eq: t -> t -> bool)
          (arr: [n]t): []t =
  dedup_param 1 1024 5 hash eq arr
    
local def partition_block [n] 't
                          (p: t -> bool)
                          (xs: [n]t) : ([n]t, [2]i64, [2]i16) =
  let pairwise op (a1, b1) (a2, b2) = (a1 `op` a2, b1 `op` b2)
  let flags =
    xs
    |> map (\x ->
              ( i16.bool <| not <| p x
              , i16.bool <| p x
              ))
  let offsets = scan (pairwise (+)) (0, 0) flags
  let (na, nb) = if n == 0 then (0, 0) else last offsets
  let f bin (a, b) =
    i64.i16 ((-1)
             + a * (i16.bool (bin == 0))
             + na * (i16.bool (bin > 0))
             + b * (i16.bool (bin == 1)))
  let bins = xs |> map (i16.bool <-< p)
  let is = map2 f bins offsets
  in ( scatter (copy xs) is xs
     , map i64.i16 [na, nb]
     , [0, na]
     )

local def exscan op ne xs =
  let s =
    scan op ne xs
    |> rotate (-1)
  let s[0] = ne
  in s

local def blocked_partition_auxiliary [n] [m] [r] 't
                                      (p: t -> bool)
                                      (xs: [n * m + r]t) : ([n * m + r]t, i64) =
  let (blocks, rest) = split xs
  let (partitioned_rest, hist_rest, offsets_rest) =
    partition_block p rest
  let (partitioned_blocks, hist_blocks, offsets_blocks) =
    unflatten blocks
    |> map (partition_block p)
    |> unzip3
  let histograms = hist_blocks ++ [hist_rest]
  let partitions = sized (n * m + r) (flatten partitioned_blocks ++ partitioned_rest)
  let old_offsets = offsets_blocks ++ [offsets_rest]
  let new_offsets =
    histograms
    |> transpose
    |> flatten
    |> exscan (+) 0
    |> unflatten
    |> transpose
  let is =
    tabulate (n * m + r) (\i ->
                            let elem = partitions[i]
                            let bin = i64.bool <| p elem
                            let block_idx = i / m
                            let new_offset = new_offsets[block_idx][bin]
                            let old_block_offset = i64.i16 old_offsets[block_idx][bin]
                            let old_offset = m * block_idx + old_block_offset
                            let idx = (i - old_offset) + new_offset
                            in idx)
  let out = replicate (n * m + r) xs[0] :> [n * m + r]t
  in (scatter out is partitions, new_offsets[0][1])

-- | The inherently evil blocked partition, this leads to a speed up
-- compared to the partition given in Futharks prelude. It works much
-- liked a blocked radix sort [1] but just with one bit.
--
-- [1] N. Satish, M. Harris and M. Garland, "Designing efficient
-- sorting algorithms for manycore GPUs," 2009 IEEE International
-- Symposium on Parallel & Distributed Processing, Rome, Italy, 2009,
-- pp. 1-10, doi: 10.1109/IPDPS.2009.5161005.
def blocked_partition [n] 't
                      (p: t -> bool)
                      (xs: [n]t) : ?[k].([k]t, [n - k]t) =
  let block = i64.i16 256
  let n_blocks = n / block
  let rest = n % block
  let xs = sized (n_blocks * block + rest) xs
  let (ys, o) = blocked_partition_auxiliary (not <-< p) xs
  in (ys[0:o], ys[o:n])

local def hash_i32 a x = hash_i64 a (i64.i32 x)

local entry replicate_i32 (n: i64) (m: i32): [n]i32 =
  replicate n m 

local entry replicate_i64 (n: i64) (m: i64): [n]i64 =
  replicate n m 

-- ==
-- entry: bench_dedup
-- compiled random input { [100000]i32 }
-- compiled random input { [1000000]i32 }
-- compiled random input { [10000000]i32 }
-- compiled random input { [100000000]i32 }
-- script input { replicate_i32 100000i64 1i32 }
-- script input { replicate_i32 1000000i64 1i32 }
-- script input { replicate_i32 10000000i64 1i32 }
-- script input { replicate_i32 100000000i64 1i32 }
local entry bench_dedup [n] (arr: [n]i32) =
  dedup hash_i32 (==) arr

-- ==
-- entry: bench_dedup_mod
-- compiled random input { [100000]i32 }
-- compiled random input { [1000000]i32 }
-- compiled random input { [10000000]i32 }
-- compiled random input { [100000000]i32 }
local entry bench_dedup_mod [n] (arr: [n]i32) =
  dedup hash_i32 (==) (map (% 40000) arr)

  
-- ==
-- entry: bench_count_occourences_i32
-- compiled random input { [100000]i32 }
-- compiled random input { [1000000]i32 }
-- compiled random input { [10000000]i32 }
-- compiled random input { [100000000]i32 }
-- script input { replicate_i32 100000i64 1i32 }
-- script input { replicate_i32 1000000i64 1i32 }
-- script input { replicate_i32 10000000i64 1i32 }
-- script input { replicate_i32 100000000i64 1i32 }
local entry bench_count_occourences_i32 [n] (arr: [n]i32) =
  reduce_by_key hash_i32 (==) 0i32 (+) <| map (\a -> (a, 1)) arr
  |> map (.0)

-- ==
-- entry: bench_count_occourences_mod_i32
-- compiled random input { [100000]i32 }
-- compiled random input { [1000000]i32 }
-- compiled random input { [10000000]i32 }
-- compiled random input { [100000000]i32 }
local entry bench_count_occourences_mod_i32 [n] (arr: [n]i32) =
  reduce_by_key hash_i32 (==) 0i32 (+) <| map (\a -> (a % 40000, 1)) arr
  |> map (.0)
  

-- Contains code from the segmented library.
-- https://github.com/diku-dk/segmented
-- ==
-- entry: bench_count_occourences_radix_sort_and_segscan_i32
-- compiled random input { [100000]i32 }
-- compiled random input { [1000000]i32 }
-- compiled random input { [10000000]i32 }
-- compiled random input { [100000000]i32 }
local entry bench_count_occourences_radix_sort_and_segscan_i32 [n] (arr: [n]i32) =
  let sorted = blocked_radix_sort 256 i32.num_bits i32.get_bit arr
  let flags =
    map (
      \i ->
        i == 0 || sorted[i - 1] != sorted[i]
    ) (iota n)
  let as = segmented_scan (+) 0 flags (replicate n 1) |> zip sorted
  let segment_ends = rotate 1 flags
  let segment_end_offsets = segment_ends |> map i64.bool |> scan (+) 0
  let num_segments = if n > 0 then last segment_end_offsets else 0
  let scratch = replicate num_segments (0, 0)
  let index i f = if f then i-1 else -1
  in scatter scratch (map2 index segment_end_offsets segment_ends) as
     |> map (.0)

-- ==
-- entry: bench_count_occourences_i64
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
-- script input { replicate_i64 100000i64 1i64 }
-- script input { replicate_i64 1000000i64 1i64 }
-- script input { replicate_i64 10000000i64 1i64 }
-- script input { replicate_i64 100000000i64 1i64 }
local entry bench_count_occourences_i64 [n] (arr: [n]i64) =
  reduce_by_key hash_i64 (==) 0i64 (+) <| map (\a -> (a, 1)) arr
  |> map (.0)

-- ==
-- entry: bench_count_occourences_mod_i64
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
local entry bench_count_occourences_mod_i64 [n] (arr: [n]i64) =
  reduce_by_key hash_i64 (==) 0i64 (+) <| map (\a -> (a % 40000, 1)) arr
  |> map (.0)
  
-- Contains code from the segmented library.
-- https://github.com/diku-dk/segmented
-- ==
-- entry: bench_count_occourences_radix_sort_and_segscan_i64
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
local entry bench_count_occourences_radix_sort_and_segscan_i64 [n] (arr: [n]i64) =
  let sorted = blocked_radix_sort 256 i64.num_bits i64.get_bit arr
  let flags =
    map (
      \i ->
        i == 0 || sorted[i - 1] != sorted[i]
    ) (iota n)
  let as = segmented_scan (+) 0 flags (replicate n 1) |> zip sorted
  let segment_ends = rotate 1 flags
  let segment_end_offsets = segment_ends |> map i64.bool |> scan (+) 0
  let num_segments = if n > 0 then last segment_end_offsets else 0
  let scratch = replicate num_segments (0, 0)
  let index i f = if f then i-1 else -1
  in scatter scratch (map2 index segment_end_offsets segment_ends) as
     |> map (.0)

-- ==
-- entry: bench_blocked_partition_i32 bench_partition_i32
-- random input { [100000]i32 }
-- random input { [1000000]i32 }
-- random input { [10000000]i32 }
-- random input { [100000000]i32 }
entry bench_blocked_partition_i32 [n] (xs: [n]i32) =
  blocked_partition (< 0) xs

entry bench_partition_i32 [n] (xs: [n]i32) =
  partition (< 0) xs
