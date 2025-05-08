import "hashset"
import "array"

def seed = engine.rng_from_seed [1]

--| A hash function that seems to work great for i64 in regards to
-- removing duplicates or reduce by key.
-- The hash function was found [here](http://stackoverflow.com/a/12996028).
def hashi64 (a: [1]i64) (x: i64) : i64 =
  let x = a[0] * x
  let x = (x ^ (x >> 30)) * (i64.u64 0xbf58476d1ce4e5b9)
  let x = (x ^ (x >> 27)) * (i64.u64 0x94d049bb133111eb)
  let y = (x ^ (x >> 31))
  in y
                                      
-- ==
-- entry: test_find_all
-- compiled random input { 10000000i64 }
-- output { true }
entry test_find_all n =
  let xs = iota n 
  let (_, set) = construct seed (==) hashi64 xs
  in all (member set) xs
  
-- ==
-- entry: test_does_not_find
-- compiled random input { 10000000i64 }
-- output { true }
entry test_does_not_find n =
  let ys = iota n
  let (_, set) = construct seed (==) hashi64 ys
  in all (not <-< member set) (n..<n+1)
