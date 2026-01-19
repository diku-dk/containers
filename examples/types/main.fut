import "../../lib/github.com/diku-dk/containers/unionfind"
import "../../lib/github.com/diku-dk/sorts/merge_sort"

type vname = i64
type tname = i64
type e = i64

type exp =
    #var vname
  | #lam vname e
  | #app e e

type typ =
    #tvar tname
  | #tarrow tname tname

type constraint = (typ, typ)

def num_type_vars (exp: exp) : i64 =
  match exp
  case #var _ -> 1
  case #lam _ _ -> 1
  case #app _ _ -> 3

def num_type_equivs (exp: exp) : i64 =
  match exp
  case #var _ -> 0
  case #lam _ _ -> 0
  case #app _ _ -> 3

def exscan f ne xs =
  map2 (\i x -> if i == 0 then ne else x)
       (indices xs)
       (rotate (-1) (scan f ne xs))

def constraint [n] [m]
               (tvs: [m]tname)
               (offsets: [n]i64)
               (exps: [n]exp)
               (seg_i: i64)
               (i: i64) : constraint =
  let o = offsets[seg_i]
  in match (exps[o], i)
     -- t(e0 e1) = tvs[o]
     -- a = tvs[o + 1]
     -- b = tvs[o + 2]
     case (#app e0 _, 0) ->
       -- t(e0) ~ a -> b
       (#tvar tvs[offsets[e0]], #tarrow tvs[o + 1] tvs[o + 2])
     case (#app _ e1, 1) ->
       -- t(e1) ~ a
       (#tvar tvs[offsets[e1]], #tvar tvs[o + 1])
     case (#app _ _, 2) ->
       -- t(e0 e1) ~ b
       (#tvar tvs[o], #tvar tvs[o + 2])
     case _ -> assert false ([] :> []constraint)[0]

def constraints [n] (exps: [n]exp) =
  let shape = map num_type_equivs exps
  let size = i64.sum shape
  let num_tvs = map num_type_vars exps
  let offsets = exscan (+) 0 num_tvs
  let tvs = i64.sum num_tvs |> iota
  let seg_is =
    scatter (replicate size 0)
            (exscan (+) 0 shape)
            (map (i64.bool <-< bool.i64) (indices offsets))
    |> scan (+) 0
  let is = tabulate size (\i -> i - offsets[seg_is[i]])
  let _ = #[trace] tabulate n (\i -> (exps[i], #tvar tvs[offsets[i]] : typ))
  in map2 (constraint tvs offsets exps) seg_is is

def cmp (t: typ) (t': typ) : bool =
  match (t, t')
  case (#tarrow _ _, #tvar _) -> false
  case (#tvar _, #tarrow _ _) -> true
  case (#tvar a, #tvar a') -> a <= a'
  case (#tarrow a b, #tarrow a' b') ->
    if a == a' then b <= b' else a <= a'

def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n - 1)
    while l < r do
      let t = l + (r - l) / 2
      in if x `lte` xs[t]
         then (l, t)
         else (t + 1, r)
  in l

def sort_constraints [m] (cons: [m]constraint) : [2 * m]typ =
  unzip cons
  |> uncurry (++)
  |> merge_sort cmp
  |> sized (2 * m)

def encode_constraints [m] (sorted_typ: [2 * m]typ) (cons: [m]constraint) =
  let search (a: typ) = binary_search cmp sorted_typ a
  in map (\(a, b) -> (search a, search b)) cons
