import "../../lib/github.com/diku-dk/containers/unionfind"

-- | Name of a expression variable.
type vname = i64

-- | Name of a type variable.
type tname = i64

-- | Index pointer to an expression.
type e = i64

-- | Lambda calculus expression of either a variable, lambda function,
-- or function application.
type exp =
    #var vname
  | #lam vname e
  | #app e e

-- | Type of a lambda calculus expression, either a type variable or
-- a type arrow.
type typ =
    #tvar tname
  | #tarrow tname tname

-- | A constraint forcing two type expressions to be equal.
type constraint = (typ, typ)

-- | All expressions is assigned a type variable name based on its
-- index.
def index_to_tname (i: i64) : tname =
  i

-- | All expression variable names is assigned a type variable based
-- on its name.
def var_to_tname (n: i64) (v: vname) : tname =
  n + v

-- | Generate a constraint based on an subexpression.
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

-- | Generate all constraints for an expression.
def constraints [n] (exps: [n]exp) =
  tabulate n (constraint exps)

-- | Less than or equal ordering on types.
def cmp (t: typ) (t': typ) : bool =
  match (t, t')
  case (#tarrow _ _, #tvar _) -> false
  case (#tvar _, #tarrow _ _) -> true
  case (#tvar a, #tvar a') -> a <= a'
  case (#tarrow a b, #tarrow a' b') ->
    if a == a' then b <= b' else a <= a'

-- | Equality on types.
def eq (t: typ) (t': typ) : bool =
  match (t, t')
  case (#tarrow _ _, #tvar _) -> false
  case (#tvar _, #tarrow _ _) -> false
  case (#tvar a, #tvar a') -> a == a'
  case (#tarrow a b, #tarrow a' b') -> a == a' && b == b'

-- | Extract variable name of expression.
def vname (exp: exp) : vname =
  match exp
  case #app _ _ -> 0
  case #lam v _ -> v
  case #var v -> v

-- | Number of type variables.
def num_tvars [n] (exps: [n]exp) : i64 =
  n + i64.maximum (map vname exps)

-- | Number of all possible types.
def num_types [n] (exp: [n]exp) : i64 =
  let n = num_tvars exp
  in n + n * n

-- | Encode a type variable to an integer.
def encode (n: i64) (t: typ) : i64 =
  match t
  case #tvar t -> t
  case #tarrow t t' -> n + t * n + t'

-- | Generate a new constraint if two arrow types are said to be
-- equal.
def new_constraints (con: constraint) : (constraint, constraint) =
  match con
  case (#tarrow t t', #tarrow t'' t''') ->
    ((#tvar t, #tvar t''), (#tvar t', #tvar t'''))
  case _ -> ((#tvar (-1), #tvar (-1)), (#tvar (-1), #tvar (-1)))

-- | Check if a type is valid.
def is_valid (t: typ) : bool =
  match t
  case #tvar t -> t != -1
  case _ -> true

-- | Shortcut for creating a type variable.
def tvar (tname: i64) : typ =
  #tvar tname

module uf = unionfind_by_rank

-- | Solve type constraints for an expression.
def solve [n] (exps: [n]exp) =
  let cons = constraints exps
  let num_ts = num_types exps
  let num_tvars = num_tvars exps
  let u = uf.create num_ts
  let types = unzip cons |> uncurry (++)
  -- Initial table of all type variables with allocated space for all
  -- type arrows.
  let table =
    map (\i -> if i < num_tvars then tvar i else tvar (-1))
        (iota num_ts)
  let table = scatter table (map (encode n) types) types
  let (u, _) =
    loop (u, cons) =
           (u, cons)
    while length cons != 0 do
      -- Encode constraints for union-find.
      let econs =
        map (\(t, t') ->
               ( uf.from_i64 u (encode n t)
               , uf.from_i64 u (encode n t')
               ))
            cons
      -- Perform unification.
      let u = uf.union u econs
      -- Find representative of left-hand side of type equality.
      let is =
        map (.0) econs
        |> uf.find' u
        |> map (uf.to_i64 u)
      -- Find new type constraints, they occur if two type arrows
      -- equal.
      let cons =
        map2 (\i (_, t) -> (table[i], t)) is cons
        |> filter (not <-< uncurry eq)
        |> map new_constraints
        |> filter (is_valid <-< (.0) <-< (.0))
        |> unzip
        |> uncurry (++)
      in (u, cons)
  -- Find type of all expression and zip them together.
  let exp_types =
    tabulate n (uf.from_i64 u)
    |> uf.find' u
    |> map (uf.to_i64 u)
    |> map (\i -> table[i])
    |> zip exps
  -- Find all non-trivial type equalities after union find.
  let type_eqs =
    tabulate num_ts (uf.from_i64 u)
    |> uf.find' u
    |> map (uf.to_i64 u)
    |> map2 (\j i -> (table[j], table[i])) (iota num_ts)
    |> filter (is_valid <-< (.1))
    |> filter (not <-< uncurry eq)
  in (exp_types, type_eqs)
