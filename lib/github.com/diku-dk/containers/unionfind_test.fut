import "unionfind"
import "../sorts/merge_sort"

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
  (E: {
    type t
    val (==) : t -> t -> bool
  }
  with t = U.handle)
  : test = {
  type t = U.handle

  def normalize [n] (hs: [n]t) : [n]i64 =
    let is =
      map (\h ->
             zip (indices hs) hs
             |> reduce_comm (\a b ->
                               if a.0 != -1 && b.0 != -1
                               then if a.1 E.== h
                                    then a
                                    else b
                               else a)
                            (-1, U.none))
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

module unionfind_seq = mk_unionfind_sequential
module unionfind = mk_unionfind

module test_unionfind_seq = mk_test unionfind_seq i64
module test_unionfind = mk_test unionfind i64

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

entry norm_eq_count_unionfind_seq [m] (n: i64) (eqs: [m](i64, i64)) : [n]i64 =
  test_unionfind_seq.norm_eq_count n eqs

-- ==
-- entry: norm_eq_count_unionfind
-- script input { let num_vars = 100i64 in
--                let num_eqs = 20i64 in
--                let eqs = equations 100i64 20i64
--                 in (100i64, eqs) }
-- script output { let num_vars = 100i64 in
--                 let num_eqs = 20i64 in
--                 let eqs = equations 100i64 20i64
--                  in norm_eq_count_unionfind_seq }
entry norm_eq_count_unionfind [m] (n: i64) (eqs: [m](i64, i64)) : [n]i64 =
  test_unionfind.norm_eq_count n eqs
