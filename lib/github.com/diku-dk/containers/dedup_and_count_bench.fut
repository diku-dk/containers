import "../cpprandom/random"
import "../sorts/radix_sort"
import "../segmented/segmented"
import "hashset"
import "array"

module i64_key = {
  type i = u64
  type k = i64

  def m : i64 = 1

  def hash (a: [m]u64) (x: i64) : i64 =
    let x = i64.u64 a[0] * x
    let x = (x ^ (x >> 30)) * (i64.u64 0xbf58476d1ce4e5b9)
    let x = (x ^ (x >> 27)) * (i64.u64 0x94d049bb133111eb)
    let y = (x ^ (x >> 31))
    in y

  def eq : i64 -> i64 -> bool = (==)
}

module engine = xorshift128plus
module array = array i64_key engine
module hashset = hashset i64_key engine
def seed = engine.rng_from_seed [1]

local
entry replicate_i64 (n: i64) (m: i64) : [n]i64 =
  replicate n m

-- ==
-- entry: bench_dedup
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
-- script input { replicate_i64 100000i64 1i64 }
-- script input { replicate_i64 1000000i64 1i64 }
-- script input { replicate_i64 10000000i64 1i64 }
-- script input { replicate_i64 100000000i64 1i64 }
local
entry bench_dedup [n] (arr: [n]i64) =
  array.dedup seed arr |> (.1)

-- ==
-- entry: bench_dedup_mod
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
local
entry bench_dedup_mod [n] (arr: [n]i64) =
  array.dedup seed (map (% 40000) arr) |> (.1)

-- ==
-- entry: bench_count_occourences
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
-- script input { replicate_i64 100000i64 1i64 }
-- script input { replicate_i64 1000000i64 1i64 }
-- script input { replicate_i64 10000000i64 1i64 }
-- script input { replicate_i64 100000000i64 1i64 }
local
entry bench_count_occourences [n] (arr: [n]i64) =
  zip arr (replicate n 1)
  |> array.reduce_by_key seed (+) 0i64
  |> (.1)
  |> unzip

-- ==
-- entry: bench_count_occourences_mod
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
local
entry bench_count_occourences_mod [n] (arr: [n]i64) =
  map (% 40000) arr
  |> flip zip (replicate n 1)
  |> array.reduce_by_key seed (+) 0i64
  |> (.1)
  |> unzip

-- Contains code from the segmented library.
-- https://github.com/diku-dk/segmented
-- ==
-- entry: bench_count_occourences_radix_sort_and_segscan
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- compiled random input { [100000000]i64 }
local
entry bench_count_occourences_radix_sort_and_segscan [n] (arr: [n]i64) =
  let sorted = blocked_radix_sort 256 i64.num_bits i64.get_bit arr
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
     |> map (.0)
