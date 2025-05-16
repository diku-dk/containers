-- | ignore

import "../cpprandom/random"
import "hashset"

-- The hash function was found [here](http://stackoverflow.com/a/12996028).
module i64_key = {
  type k = i64
  type ctx = ()

  def m : i64 = 1

  def hash _ (a: [m]u64) (x: i64) : u64 =
    let x = a[0] * u64.i64 x
    let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
    let x = (x ^ (x >> 27)) * 0x94d049bb133111eb
    let y = (x ^ (x >> 31))
    in y

  def eq _ x _ y = x i64.== y
}

module engine = xorshift128plus
module hashset = mk_hashset i64_key engine
def seed = engine.rng_from_seed [1]

-- ==
-- entry: test_find_all
-- compiled random input { 1000000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_find_all n =
  let xs = iota n
  let (_, s) = hashset.from_array () seed xs
  in all (\x -> hashset.member () x s) xs

-- ==
-- entry: test_does_not_find
-- compiled random input { 1000000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_does_not_find n =
  let ys = iota n
  let (_, s) = hashset.from_array () seed ys
  let idxs = (n..<n + 1)
  in all (\x -> hashset.not_member () x s) idxs

-- ==
-- entry: test_find_all_dups
-- compiled random input { 10000i64 }
-- output { true }
entry test_find_all_dups n =
  let xs = iota n |> map (% 10)
  let (_, s) = hashset.from_array () seed xs
  in all (\x -> hashset.member () x s) xs

-- ==
-- entry: test_does_not_find_dups
-- compiled random input { 10000i64 }
-- output { true }
entry test_does_not_find_dups n =
  let ys = iota n |> map (% 10)
  let (_, s) = hashset.from_array () seed ys
  let idxs = (10..<n)
  in all (\x -> hashset.not_member () x s) idxs

-- ==
-- entry: test_dedup
-- compiled random input { 100000i64 }
-- output { true }
entry test_dedup n =
  let ys = iota n |> map (% 100)
  let (_, s) = hashset.from_array () seed ys
  let arr = hashset.to_array s
  in length arr == 100 && all (\a -> or (map (== a) (iota 100))) arr
