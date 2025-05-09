import "hashset_unlifted"
import "../cpprandom/random"
                           
--| A hash function that seems to work great for i64 in regards to
-- removing duplicates or reduce by key.
-- The hash function was found [here](http://stackoverflow.com/a/12996028).
module i64_key = {
  type i = u64
  type k = i64
           
  def m: i64 = 1
           
  def hash (a: [m]u64) (x: i64) : i64 =
    let x = i64.u64 a[0] * x
    let x = (x ^ (x >> 30)) * (i64.u64 0xbf58476d1ce4e5b9)
    let x = (x ^ (x >> 27)) * (i64.u64 0x94d049bb133111eb)
    let y = (x ^ (x >> 31))
    in y

  def eq: i64 -> i64 -> bool = (==)
}


module engine = xorshift128plus
module set = hashset i64_key engine
def seed = engine.rng_from_seed [1]

-- ==
-- entry: test_find_all
-- compiled random input { 10000000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_find_all n =
  let xs = iota n 
  let (_, s) = set.from_array seed xs
  in all (flip set.member s) xs
  
-- ==
-- entry: test_does_not_find
-- compiled random input { 10000000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_does_not_find n =
  let ys = iota n
  let (_, s) = set.from_array seed ys
  in all (flip set.not_member s) (n..<n+1)

-- ==
-- entry: test_find_all_dups
-- compiled random input { 10000i64 }
-- output { true }
entry test_find_all_dups n =
  let xs = iota n |> map (%10)
  let (_, s) = set.from_array seed xs
  in all (flip set.member s) xs

-- ==
-- entry: test_does_not_find_dups
-- compiled random input { 10000i64 }
-- output { true }
entry test_does_not_find_dups n =
  let ys = iota n |> map (%10)
  let (_, s) = set.from_array seed ys
  in all (flip set.not_member s) (10..<n)

-- ==
-- entry: test_dedup
-- compiled random input { 100000i64 }
-- output { true }
entry test_dedup n =
  let ys = iota n |> map (%100)
  let (_, s) = set.from_array seed ys
  let arr = set.to_array s
  in length arr == 100 && all (\a -> or (map (==a) (iota 100))) arr
