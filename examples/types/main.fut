import "../../lib/github.com/diku-dk/containers/unionfind"
import "../../lib/github.com/diku-dk/segmented/segmented"

type vname = i64
type tname = i64
type e = i64
type t = i64

type exp =
    #var vname
  | #lam vname e
  | #app e e

type typ =
    #tvar tname
  | #tarrow t t

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

def type_var_ranges [n] (exps: [n]exp) : [n](tname, tname) =
  let num_vars = map num_type_vars exps
  let starts = scan (+) 0 num_vars
  in map2 (\i s ->
             if i == 0 then (0, s) else (starts[i - 1], starts[i]))
          (iota n)
          starts

def constraint [n]
               (exps_vars: [n]tname)
               (e: exp)
               (t: tname) : constraint =
  let a = 0
  let b = 1
  let c = 2
  in match (e, t)
     case (#app _ _, 0) -> (#tvar a, #tarrow b c)
     case (#app e0 _, 1) -> (#tvar b, #tvar exps_vars[e0])
     case (#app _ e1, 2) -> (#tvar c, #tvar exps_vars[e1])
     case _ -> assert false ([] :> []constraint)[0]

def constraints [n] (exps: [n]exp) : []constraint =
  let ranges = type_var_ranges exps
  let exps_type_vars = map (.0) ranges
  let _repiota = replicated_iota (map (\(t, t') -> t' - t) ranges)
  in [][0]
