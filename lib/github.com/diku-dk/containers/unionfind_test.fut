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

entry norm_eq_count_unionfind_seq [m] (n: i64) (eqs: [m](i64, i64)) : [n]i64 =
  test_unionfind_seq.norm_eq_count n eqs

entry norm_eq_count_unionfind [m] (n: i64) (eqs: [m](i64, i64)) : [n]i64 =
  test_unionfind.norm_eq_count n eqs
