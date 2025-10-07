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
              (lte: t -> t -> bool)
              (f: t -> t)
              (hs: [n]t) : [n]i64 =
  let (is, hs') =
    map f hs
    |> zip (iota n)
    |> merge_sort (\(a0, b0) (a1, b1) ->
                     if a0 == a1 then b0 `lte` b1 else a0 <= a1)
    |> unzip
  let js = map (binary_search lte hs') hs
  in map (\i -> is[i]) js

entry count_equal_elems [m] (n: i64) (sample: [m]i64) : [n]i64 =
  let (uf, hs) = unionfind.create n
  let eqs = equations hs sample
  let uf' = unionfind.union (copy uf) eqs
  let reps = normalize (<=) (unionfind.find uf') hs
  in hist (+) 0 n reps (rep 1)

entry count_equal_elems_seq [m] (n: i64) (sample: [m]i64) : [n]i64 =
  let (uf_seq, hs_seq) = unionfind_seq.create n
  let eqs_seq = equations hs_seq sample
  let uf_seq' = unionfind_seq.union (copy uf_seq) eqs_seq
  let reps_seq = normalize (<=) (unionfind_seq.find uf_seq') hs_seq
  in hist (+) 0 n reps_seq (rep 1)

-- ==
-- entry: test
-- compiled random input { 10000i64 [2000]i64 }
-- output { true }
entry test [m] (n: i64) (sample: [m]i64) =
  let reps_count = count_equal_elems n sample
  let reps_seq_count = count_equal_elems_seq n sample
  in map2 (==) reps_count reps_seq_count |> and
