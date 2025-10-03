import "unionfind"

module unionfind_seq = mk_unionfind_sequential
module unionfind = mk_unionfind

def equations [m] [n] 'h (hs: [m]h) (ds: [n]i64) =
  let is = map (% m) ds
  let hs2 = map (\i -> hs[i]) is
  in tabulate (n / 2) (\i -> (hs2[2 * i], hs2[2 * i + 1]))

-- compiled input { 1000i64 [200]i64 }
-- output { true }
entry test [m] (n: i64) (arr: [m]i64) =
  let (uf, hs) = unionfind.create n
  let hs_to_is = zip hs (iota n)
  let eqs = equations hs arr
  let uf = unionfind.union uf eqs
  let (uf', hs') = unionfind_seq.create n
  let hs_to_is' = zip hs' (iota n)
  let eqs' = equations hs' arr
  let uf' = unionfind_seq.union uf' eqs'
  in hs
