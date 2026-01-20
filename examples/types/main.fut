import "../../lib/github.com/diku-dk/containers/unionfind"
import "../../lib/github.com/diku-dk/sorts/merge_sort"

def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n - 1)
    while l < r do
      let t = l + (r - l) / 2
      in if x `lte` xs[t]
         then (l, t)
         else (t + 1, r)
  in l

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

def var_to_tname (n: i64) (v: vname) : tname =
  n + v

def index_to_tname (i: i64) : tname =
  i

def constraint [n]
               (exps: [n]exp)
               (i: i64) : constraint =
  match exps[i]
  case #app e0 e1 ->
    -- t(e0) ~ t(e1) -> t(e0 e1)
    (#tvar (index_to_tname e0), #tarrow (index_to_tname e1) (index_to_tname i))
  case #lam v e ->
    -- t(\v -> e) ~ t(v) -> t(e)
    (#tvar (index_to_tname i), #tarrow (var_to_tname n v) (index_to_tname e))
  case #var v ->
    -- t(v) ~ t(e)
    (#tvar (index_to_tname i), #tvar (var_to_tname n v))

def constraints [n] (exps: [n]exp) =
  tabulate n (constraint exps)

def cmp (t: typ) (t': typ) : bool =
  match (t, t')
  case (#tarrow _ _, #tvar _) -> false
  case (#tvar _, #tarrow _ _) -> true
  case (#tvar a, #tvar a') -> a <= a'
  case (#tarrow a b, #tarrow a' b') ->
    if a == a' then b <= b' else a <= a'

def sort_constraints [m] (cons: [m]constraint) : [2 * m]typ =
  unzip cons
  |> uncurry (++)
  |> merge_sort cmp
  |> sized (2 * m)

def encode_constraints [m] (sorted_typ: [2 * m]typ) (cons: [m]constraint) =
  let search (a: typ) = binary_search cmp sorted_typ a
  in map (\(a, b) -> (search a, search b)) cons
