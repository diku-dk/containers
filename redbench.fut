import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/containers/hashset"
import "lib/github.com/diku-dk/containers/array"
import "lib/github.com/diku-dk/containers/hashmap"
import "lib/github.com/diku-dk/containers/key"
import "lib/github.com/diku-dk/containers/hash"

module array_key = mk_array_key i64key
module hashset = mk_hashset i64key
module hashmap = mk_hashmap i64key

def seed = i64key.rng_from_seed [1]

def const = (i64key.rand seed).1

local
entry replicate_i64 (n: i64) (m: i64) : [n]i64 =
  replicate n m

local
entry mod_i64 (n: i64) (m: i64) : [n]i64 =
  iota n
  |> map ((% m) <-< i64.u64 <-< i64key.hash () const)

local
def sort_dedup [n] (arr: [n]i64) : []i64 =
  let sorted = radix_sort_int i64.num_bits i64.get_bit arr
  let flags =
    map (\i ->
           i == 0 || sorted[i - 1] != sorted[i])
        (iota n)
  in zip flags sorted
     |> filter (.0)
     |> map (.1)

local
def sort_count_occourences [n] (arr: [n]i64) : [](i64, i64) =
  let sorted = radix_sort_int i64.num_bits i64.get_bit arr
  let flags =
    map (\i ->
           i == 0 || sorted[i - 1] != sorted[i])
        (iota n)
  let as = segmented_scan (+) 0 flags (replicate n 1) |> zip sorted
  let segment_ends = rotate 1 flags
  let segment_end_offsets = segment_ends |> map i64.bool |> scan (+) 0
  let num_segments = if n > 0 then last segment_end_offsets else 0
  let scratch = replicate num_segments (0, 0)
  let index i f = if f then i - 1 else -1
  in scatter scratch (map2 index segment_end_offsets segment_ends) as

-- ==
-- entry: bench_array_dedup bench_array_count_occourences bench_hashset_dedup bench_hashmap_count_occourences
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
-- script input { mod_i64 100000i64 40000i64 }
-- script input { mod_i64 1000000i64 40000i64 }
-- script input { mod_i64 10000000i64 40000i64 }
-- script input { mod_i64 100000000i64 40000i64 }
-- script input { replicate_i64 100000i64 1i64 }
-- script input { replicate_i64 1000000i64 1i64 }
-- script input { replicate_i64 10000000i64 1i64 }
-- script input { replicate_i64 100000000i64 1i64 }
local
entry bench_array_dedup [n] (arr: [n]i64) =
  array_key.dedup () seed arr |> (.1)

local
entry bench_array_count_occourences [n] (arr: [n]i64) =
  zip arr (replicate n 1)
  |> array_key.reduce_by_key () seed (+) 0i64
  |> (.1)
  |> unzip

local
entry bench_hashset_dedup [n] (arr: [n]i64) =
  hashset.from_array () arr |> hashset.to_array

local
entry bench_hashmap_count_occourences [n] (arr: [n]i64) =
  zip arr (replicate n 1)
  |> hashmap.from_array_hist () (+) 0
  |> hashmap.to_array
  |> map (.0)

-- Contains code from the segmented library.
-- https://github.com/diku-dk/segmented
-- ==
-- entry: bench_sort_dedup bench_sort_count_occourences
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
local
entry bench_sort_dedup [n] (arr: [n]i64) =
  sort_dedup arr

local
entry bench_sort_count_occourences [n] (arr: [n]i64) =
  sort_count_occourences arr |> map (.1)
