import "unionfind"
import "../sorts/merge_sort"
import "opt"

module type test = {
  -- | Normalized equivalence count will create an initial union find
  -- structures of size `n`@norm_eq_count then uses the
  -- `eqs`@norm_eq_count to index into the array of handles to create
  -- the array of equations that are used to be unioned. Then the
  -- number of equivalent elements are counted where the element with
  -- the smallest index is used to be the representative.
  val norm_eq_count [m] : (n: i64) -> (eqs: [m](i64, i64)) -> [n]i64
}

module mk_test
  (U: unionfind)
  : test = {
  type t = U.handle

  def normalize [n] (hs: [n]t) : [n]i64 =
    let eq = equal_opt (U.==)
    let is =
      map (\h ->
             map some hs
             |> zip (indices hs)
             |> reduce_comm (\a b ->
                               if a.0 != -1 && b.0 != -1
                               then if a.1 `eq` some h
                                    then a
                                    else b
                               else a)
                            (-1, #none))
          hs
      |> map (.0)
    in is

  def norm_eq_count [m]
                    (n: i64)
                    (eqs: [m](i64, i64)) : [n]i64 =
    let (uf, hs) = U.create n
    let eqs' = map (\(i, j) -> (hs[i % n], hs[j % n])) eqs
    let uf = U.union (copy uf) eqs'
    let (_, ps) = U.find uf hs
    let reps = normalize ps
    in hist (+) 0 n reps (rep 1)
}

module test_unionfind_sequential = mk_test unionfind_sequential
module test_unionfind = mk_test unionfind
module test_unionfind_by_size = mk_test unionfind_by_size
module test_unionfind_by_rank = mk_test unionfind_by_rank

-- | Multiply-shift hash function https://arxiv.org/abs/1504.06804
def hash (a: (u64, u64)) (b: (u64, u64)) (x: u64) : u64 =
  let y_mul_lo = a.0 * x
  in u64.mul_hi a.1 x + b.1 + u64.bool (y_mul_lo < b.0)

entry random (n: i64) : [n]i64 =
  let seed = (n + 2677) * 27644437
  let a0 = 0x8422d1795f837e8b
  let a1 = 0x78f81f96f93e6ca9
  let b0 = 0x643518302a112aa1
  let b1 = 0x58fe49a7f968fbe7
  in iota n
     |> map (i64.u64
             <-< hash (a0, a1) (b0, b1)
             <-< u64.i64
             <-< (+ seed))

entry equations (n: i64) (m: i64) : [m](i64, i64) =
  random (m + m)
  |> map (% n)
  |> split
  |> uncurry zip

-- ==
-- entry: norm_eq_count_unionfind
-- input { 10000i64 2000i64 }
-- output { true }
entry norm_eq_count_unionfind (num_vars: i64) (num_eqs: i64) : bool =
  let eqs = equations num_vars num_eqs
  let expected = test_unionfind_sequential.norm_eq_count num_vars eqs
  let result = test_unionfind.norm_eq_count num_vars eqs
  in map2 (==) expected result
     |> and

-- ==
-- entry: norm_eq_count_unionfind_by_size
-- input { 10000i64 2000i64 }
-- output { true }
entry norm_eq_count_unionfind_by_size (num_vars: i64) (num_eqs: i64) : bool =
  let eqs = equations num_vars num_eqs
  let expected = test_unionfind_sequential.norm_eq_count num_vars eqs
  let result = test_unionfind_by_size.norm_eq_count num_vars eqs
  in map2 (==) expected result
     |> and

-- ==
-- entry: norm_eq_count_unionfind_by_rank
-- input { 10000i64 2000i64 }
-- output { true }
entry norm_eq_count_unionfind_by_rank (num_vars: i64) (num_eqs: i64) : bool =
  let eqs = equations num_vars num_eqs
  let expected = test_unionfind_sequential.norm_eq_count num_vars eqs
  let result = test_unionfind_by_rank.norm_eq_count num_vars eqs
  in map2 (==) expected result
     |> and
