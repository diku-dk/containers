-- | ignore

import "../cpprandom/random"
import "arraymap"
import "ordkey"
import "opt"

module arraymap = mk_arraymap i64_key

-- ==
-- entry: test_find_all
-- compiled random input { 100000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_find_all n =
  let vs = map (+ 10) (iota n)
  let xs = iota n
  let h = arraymap.from_array () (zip xs vs)
  let v = map (from_opt (-1) <-< \x -> arraymap.lookup () x h) xs
  in all (\x -> arraymap.member () x h) xs
     && all (\x -> any (== x) v) vs

-- ==
-- entry: test_does_not_find
-- compiled random input { 100000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_does_not_find n =
  let xs = iota n
  let h = arraymap.from_array () (zip xs (iota n))
  let idxs = (n..<n + 1)
  let v = map (from_opt (-1) <-< \x -> arraymap.lookup () x h) idxs
  in all (\x -> arraymap.not_member () x h) idxs
     && all (== (-1)) v

-- ==
-- entry: test_find_all_dups
-- compiled random input { 1000i64 }
-- output { true }
entry test_find_all_dups n =
  let xs = iota n |> map (% 10)
  let h = arraymap.from_array () (zip xs (iota n))
  in all (\x -> arraymap.member () x h) xs

-- ==
-- entry: test_does_not_find_dups
-- compiled random input { 1000i64 }
-- output { true }
entry test_does_not_find_dups n =
  let m = 10
  let xs = iota n |> map (% m)
  let h = arraymap.from_array () (zip xs (iota n))
  let idxs = (m..<n)
  in all (\x -> arraymap.not_member () x h) idxs

-- ==
-- entry: test_dedup
-- compiled random input { 1000i64 }
-- output { true }
entry test_dedup n =
  let m = 50
  let xs = iota n |> map (% m)
  let h = arraymap.from_array () (zip xs (iota n))
  let arr = arraymap.to_array h |> map (.0)
  in length arr == m
     && all (\a -> or (map (== a) (iota m))) arr

-- ==
-- entry: test_hist
-- compiled random input { [1000]i64 }
-- output { true }
entry test_hist [m] (xs: [m]i64) =
  let n = 50
  let ks = map (% n) xs
  let h = arraymap.from_array_hist () (+) 0 (zip ks (replicate m 1i64))
  let (is, vs) = arraymap.to_array h |> unzip
  let res = scatter (replicate n 0i64) is vs
  let expected = hist (+) 0i64 n ks (replicate m 1i64)
  in and (map2 (==) res expected)

-- ==
-- entry: test_map
-- compiled random input { 1000i64 }
-- output { true }
entry test_map n =
  let xs = iota n
  let h = arraymap.from_array () (zip xs xs)
  let p = arraymap.map (* 2) h |> arraymap.to_array
  in all (\(k, v) -> k * 2 == v) p

-- ==
-- entry: test_update
-- compiled random input { 1000i64 }
-- output { true }
entry test_update n =
  let xs = iota n
  let h = arraymap.from_array () (zip xs xs)
  let p =
    zip xs (replicate n (-1))
    |> arraymap.update h
    |> arraymap.to_array
  in all ((== (-1)) <-< (.1)) p

-- ==
-- entry: test_insert
-- compiled random input { 1000i64 }
-- output { true }
entry test_insert n =
  let xs = iota n
  let h = arraymap.from_array () (zip xs xs)
  let p =
    zip (iota (2 * n)) (replicate (2 * n) (-1))
    |> arraymap.insert () h
    |> arraymap.to_array
  in all ((== (-1)) <-< (.1)) p && (length p == n * 2)

-- ==
-- entry: test_insert_hist
-- compiled random input { 1000i64 }
-- output { true }
entry test_insert_hist n =
  let xs = iota n
  let h = arraymap.from_array () (zip xs (replicate n 1))
  let p =
    zip (iota (2 * n)) (replicate (2 * n) 1)
    |> arraymap.insert_hist () (+) 0 h
    |> arraymap.to_array
    |> map (.1)
  in i64.sum p == n * 3
