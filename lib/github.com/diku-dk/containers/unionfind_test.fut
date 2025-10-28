import "unionfind"
import "../sorts/merge_sort"

module unionfind_seq = mk_unionfind_sequential
module unionfind = mk_unionfind

def equations [m] [n] 'h (hs: [m]h) (ds: [n]i64) =
  let is = map (% m) ds
  let hs2 = map (\i -> hs[i]) is
  in tabulate (n / 2) (\i -> (hs2[2 * i], hs2[2 * i + 1]))

-- https://futhark-lang.org/examples/binary-search.html
def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n - 1)
    while l < r do
      let t = l + (r - l) / 2
      in if x `lte` xs[t]
         then (l, t)
         else (t + 1, r)
  in l

-- Goes through every representative and finds the smallest one.
def normalize [n] 't
              (eq: t -> t -> bool)
              (none: t)
              (f: t -> t)
              (hs: [n]t) : [n]i64 =
  let is =
    map (\h ->
           zip (indices hs) hs
           |> reduce_comm (\a b ->
                             if a.0 != -1 && b.0 != -1
                             then if (f a.1) `eq` (f h)
                                  then a
                                  else b
                             else a)
                          (-1, none))
        hs
    |> map (.0)
  in is

entry count_equal_elems [m] (n: i64) (sample: [m]i64) : [n]i64 =
  let (uf, hs) = unionfind.create n
  let eqs = equations hs sample
  let uf' = unionfind.union (copy uf) eqs
  let reps = normalize (==) unionfind.none (unionfind.find uf') hs
  in hist (+) 0 n reps (rep 1)

entry count_equal_elems_seq [m] (n: i64) (sample: [m]i64) : [n]i64 =
  let (uf_seq, hs_seq) = unionfind_seq.create n
  let eqs_seq = equations hs_seq sample
  let uf_seq' = unionfind_seq.union (copy uf_seq) eqs_seq
  let reps_seq = normalize (==) unionfind.none (unionfind_seq.find uf_seq') hs_seq
  in hist (+) 0 n reps_seq (rep 1)

-- ==
-- entry: test
-- compiled random input { 100000i64 [100000]i64 }
-- output { true }
entry test [m] (n: i64) (sample: [m]i64) =
  let reps_count = count_equal_elems n sample
  let reps_seq_count = count_equal_elems_seq n sample
  in map2 (==) reps_count reps_seq_count |> and
