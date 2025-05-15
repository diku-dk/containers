-- | ignore

import "../cpprandom/random"
import "hashmap"
import "opt"

-- The hash function was found [here](http://stackoverflow.com/a/12996028).
module i64_key = {
  type i = u64
  type k = i64
  type ctx = ()

  def m : i64 = 1

  def hash _ (a: [m]u64) (x: i64) : u64 =
    let x = a[0] * u64.i64 x
    let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
    let x = (x ^ (x >> 27)) * 0x94d049bb133111eb
    let y = (x ^ (x >> 31))
    in y

  def eq _ : i64 -> i64 -> bool = (==)
}

module engine = xorshift128plus
module hashmap = mk_hashmap i64_key engine
def seed = engine.rng_from_seed [1]

-- ==
-- entry: test_find_all
-- compiled random input { 100000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_find_all n =
  let vs = map (+ 10) (iota n)
  let xs = iota n
  let (_, h) = hashmap.from_array () seed (zip xs vs)
  let v = map (from_opt (-1) <-< \x -> hashmap.lookup x h) xs
  in all (\x -> hashmap.member x h) xs
     && all (\x -> any (== x) v) vs

-- ==
-- entry: test_does_not_find
-- compiled random input { 100000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_does_not_find n =
  let xs = iota n
  let (_, h) = hashmap.from_array () seed (zip xs (iota n))
  let idxs = (n..<n + 1)
  let v = map (from_opt (-1) <-< \x -> hashmap.lookup x h) idxs
  in all (\x -> hashmap.not_member x h) idxs
     && all (== (-1)) v

-- ==
-- entry: test_find_all_dups
-- compiled random input { 1000i64 }
-- output { true }
entry test_find_all_dups n =
  let xs = iota n |> map (% 10)
  let (_, h) = hashmap.from_array () seed (zip xs (iota n))
  in all (\x -> hashmap.member x h) xs

-- ==
-- entry: test_does_not_find_dups
-- compiled random input { 1000i64 }
-- output { true }
entry test_does_not_find_dups n =
  let m = 10
  let xs = iota n |> map (% m)
  let (_, h) = hashmap.from_array () seed (zip xs (iota n))
  let idxs = (m..<n)
  in all (\x -> hashmap.not_member x h) idxs

-- ==
-- entry: test_dedup
-- compiled random input { 1000i64 }
-- output { true }
entry test_dedup n =
  let m = 50
  let xs = iota n |> map (% m)
  let (_, h) = hashmap.from_array () seed (zip xs (iota n))
  let arr = hashmap.to_array h |> map (.0)
  in length arr == m
     && all (\a -> or (map (== a) (iota m))) arr

-- ==
-- entry: test_hist
-- compiled random input { [1000]i64 }
-- output { true }
entry test_hist [m] (xs: [m]i64) =
  let n = 50
  let ks = map (% n) xs
  let (_, h) =
    hashmap.from_array_hist ()
                            seed
                            (+)
                            0
                            (zip ks (replicate m 1i64))
  let (is, vs) = hashmap.to_array h |> unzip
  let res = scatter (replicate n 0i64) is vs
  let expected = hist (+) 0i64 n ks (replicate m 1i64)
  in and (map2 (==) res expected)

-- ==
-- entry: test_map
-- compiled random input { 1000i64 }
-- output { true }
entry test_map n =
  let xs = iota n
  let (_, h) = hashmap.from_array () seed (zip xs xs)
  let p = hashmap.map (* 2) h |> hashmap.to_array
  in all (\(k, v) -> k * 2 == v) p

-- ==
-- entry: test_update
-- compiled random input { 1000i64 }
-- output { true }
entry test_update n =
  let xs = iota n
  let (_, h) = hashmap.from_array () seed (zip xs xs)
  let p =
    hashmap.update (zip xs (replicate n (-1))) h
    |> hashmap.to_array
  in all ((== (-1)) <-< (.1)) p
