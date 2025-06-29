import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/containers/hashset"
import "lib/github.com/diku-dk/containers/key"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/containers/hash"

module hashset = mk_hashset i64key

module hashset_u32 = mk_hashset_u32 i64key_u32

def seed = i64key.rng_from_seed [1]

def const = (i64key.rand seed).1

def clamp a min max =
  i64.max min (i64.min a max)

def binary_search [n] 't (eq: t -> t -> bool) (lte: t -> t -> bool) (xs: [n]t) (x: t) : bool =
  let (l, _) =
    loop (l, r) = (0, n - 1)
    while l < r do
      let t = l + (r - l) / 2
      in if x `lte` xs[t]
         then (l, t)
         else (t + 1, r)
  in xs[l] `eq` x

def log2 x = 63 - i64.clz x

def eytzinger_index (n: i64) (i: i64) =
  let lvl = log2 (i + 1)
  let offset = i64.i32 (1 << (log2 n - lvl))
  let k = i64.i32 ((1 << lvl) - 1)
  in offset + (i - k) * offset * 2 - 1

def eytzinger [n] 't (xs: [n]t) : ?[m].[m]t =
  let m = 2 ** (i64.num_bits - i64.clz n |> i64.i32) - 1
  let dest = replicate m xs[n - 1]
  let xs' = scatter dest (indices xs) xs
  let f i = xs'[eytzinger_index m i]
  in tabulate m f

def ffs x = i64.ctz x + 1

def eytzinger_search [n] 't (eq: t -> t -> bool) (lte: t -> t -> bool) (xs: [n]t) (x: t) : bool =
  let k =
    loop k = 1
    while k <= n do
      if x `lte` xs[k - 1]
      then 2 * k
      else 2 * k + 1
  let i = (k >> i64.i32 (ffs (!k))) - 1
  in 0 <= i && i < n && eq xs[i] x

local
entry replicate_i64 (n: i64) (m: i64) : [n]i64 =
  replicate n m

local
entry mod_i64 (n: i64) (m: i64) : [n]i64 =
  iota n
  |> map ((% m) <-< i64.u64 <-< i64key.hash () const)

def construct_hashset arr =
  hashset.from_array () arr

def construct_hashset_u32 arr =
  hashset_u32.from_array () arr

def construct_eytzinger arr =
  blocked_radix_sort_int 256 i64.num_bits i64.get_bit arr
  |> eytzinger

def construct_sort [n] (arr: [n]i64) : [n]i64 =
  blocked_radix_sort_int 256 i64.num_bits i64.get_bit arr

-- ==
-- entry: bench_sort_construct bench_eytzinger_construct
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
entry bench_sort_construct [n] (arr: [n]i64) =
  construct_sort arr

entry bench_eytzinger_construct [n] (arr: [n]i64) =
  construct_eytzinger arr

-- ==
-- entry: bench_hashset_construct bench_hashset_u32_construct
-- compiled random input { [100000]i64 }
-- compiled random input { [1000000]i64 }
-- compiled random input { [10000000]i64 }
-- script input { mod_i64 100000i64 40000i64 }
-- script input { mod_i64 1000000i64 40000i64 }
-- script input { mod_i64 10000000i64 40000i64 }
-- script input { replicate_i64 100000i64 1i64 }
-- script input { replicate_i64 1000000i64 1i64 }
-- script input { replicate_i64 10000000i64 1i64 }
entry bench_hashset_construct [n] (arr: [n]i64) =
  construct_hashset arr
  |> hashset.to_array

entry bench_hashset_u32_construct [n] (arr: [n]i64) =
  construct_hashset_u32 arr
  |> hashset_u32.to_array

local
entry random_array_i64 (n: i64) : []i64 =
  iota n
  |> map (i64.u64 <-< i64key.hash () const)

local
entry construct_random_sort n =
  let arr = random_array_i64 n
  in (arr, construct_sort arr)

local
entry construct_random_eytzinger n =
  let arr = random_array_i64 n
  in (arr, construct_eytzinger arr)

local
entry construct_random_hashset (n: i64) : ([]i64, hashset.set []) =
  let arr = random_array_i64 n
  in (arr, construct_hashset arr)

local
entry construct_random_hashset_u32 (n: i64) : ([]i64, hashset_u32.set []) =
  let arr = random_array_i64 n
  in (arr, construct_hashset_u32 arr)

-- ==
-- entry: bench_sort_lookup
-- script input { construct_random_sort 100000i64 }
-- script input { construct_random_sort 1000000i64 }
-- script input { construct_random_sort 10000000i64 }
entry bench_sort_lookup (arr: []i64) (arr_sorted: []i64) =
  map (binary_search (==) (<=) arr_sorted) arr

-- ==
-- entry: bench_eytzinger_lookup
-- script input { construct_random_eytzinger 100000i64 }
-- script input { construct_random_eytzinger 1000000i64 }
-- script input { construct_random_eytzinger 10000000i64 }
entry bench_eytzinger_lookup (arr: []i64) (arr_tree: []i64) =
  map (eytzinger_search (==) (<=) arr_tree) arr

-- ==
-- entry: bench_hashset_lookup
-- script input { construct_random_hashset 100000i64 }
-- script input { construct_random_hashset 1000000i64 }
-- script input { construct_random_hashset 10000000i64 }
entry bench_hashset_lookup (arr: []i64) (set: hashset.set []) =
  map (\k -> hashset.member () k set) arr

-- ==
-- entry: bench_hashset_u32_lookup
-- script input { construct_random_hashset_u32 100000i64 }
-- script input { construct_random_hashset_u32 1000000i64 }
-- script input { construct_random_hashset_u32 10000000i64 }
entry bench_hashset_u32_lookup (arr: []i64) (set: hashset_u32.set []) =
  map (\k -> hashset_u32.member () k set) arr
