import "../../lib/github.com/diku-dk/containers/unionfind"

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

def eq (t: typ) (t': typ) : bool =
  match (t, t')
  case (#tarrow _ _, #tvar _) -> false
  case (#tvar _, #tarrow _ _) -> false
  case (#tvar a, #tvar a') -> a == a'
  case (#tarrow a b, #tarrow a' b') -> a == a' && b == b'

def max_type (t: typ) (t': typ) : typ =
  if t `cmp` t' then t' else t

def vname (exp: exp) : vname =
  match exp
  case #app _ _ -> 0
  case #lam v _ -> v
  case #var v -> v

def num_tvars [n] (exps: [n]exp) : i64 =
  n + i64.maximum (map vname exps)

def num_types [n] (exp: [n]exp) : i64 =
  let n = num_tvars exp
  in n + n * n

def encode (n: i64) (t: typ) : i64 =
  match t
  case #tvar t -> t
  case #tarrow t t' -> n + t * n + t'

def new_constraints (con: constraint) : (constraint, constraint) =
  match con
  case (#tarrow t t', #tarrow t'' t''') ->
    ((#tvar t, #tvar t''), (#tvar t', #tvar t'''))
  case _ -> ((#tvar (-1), #tvar (-1)), (#tvar (-1), #tvar (-1)))

def is_invalid (t: typ) : bool =
  match t
  case #tvar t -> t == -1
  case _ -> false

module uf = unionfind_by_rank

def solve [n] (exps: [n]exp) =
  let cons = constraints exps
  let num_ts = num_types exps
  let u = uf.create num_ts
  let types = unzip cons |> uncurry (++)
  let table =
    scatter (replicate num_ts (#tvar (-1))) (map (encode n) types) types
  let table = scatter table (iota n) (tabulate n (\i -> #tvar i))
  let (u, _) =
    loop (u, cons) =
           (u, cons)
    while length cons != 0 do
      let econs =
        map (\(t, t') ->
               ( uf.from_i64 u (encode n t)
               , uf.from_i64 u (encode n t')
               ))
            cons
      let u = uf.union u econs
      let types = unzip cons |> uncurry (++)
      let is =
        map (\t -> uf.from_i64 u (encode n t)) types
        |> uf.find' u
        |> map (uf.to_i64 u)
        |> split
        |> (.0)
      let cons =
        map2 (\i (_, t) -> (table[i], t)) is cons
        |> filter (not <-< uncurry eq)
        |> map new_constraints
        |> filter (not <-< is_invalid <-< (.0) <-< (.0))
        |> unzip
        |> uncurry (++)
      in (u, cons)
  let exp_types =
    tabulate n (uf.from_i64 u)
    |> uf.find' u
    |> map (uf.to_i64 u)
    |> map (\i -> table[i])
    |> zip exps
  let type_eqs =
    tabulate num_ts (uf.from_i64 u)
    |> uf.find' u
    |> map (uf.to_i64 u)
    |> map2 (\j i -> (table[j], table[i])) (iota num_ts)
    |> filter (not <-< is_invalid <-< (.1))
    |> filter (not <-< uncurry eq)
  in (exp_types, type_eqs)
