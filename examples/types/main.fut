import "../../lib/github.com/diku-dk/containers/unionfind"

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
     case (#app _ _, 0) ->
       (#tvar tvs[o + 0], #tarrow tvs[o + 1] tvs[o + 2])
     case (#app e0 _, 1) ->
       (#tvar tvs[o + 1], #tvar tvs[offsets[e0] + 0])
     case (#app _ e1, 2) ->
       (#tvar tvs[o + 2], #tvar tvs[offsets[e1] + 0])
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
  let tvars =
    #[trace]
    exscan (+) 0 num_tvs
    |> map2 (\e o -> (e, #tvar tvs[o] : typ)) exps
  in map2 (constraint tvs offsets exps) seg_is is
