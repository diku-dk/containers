import "hashkey"
import "hashmap"
import "opt"

module type unionfind = {
  type handle
  type unionfind [n]
  val none : handle
  val create : (n: i64) -> (unionfind [n], [n]handle)
  val union [n] [u] : *unionfind [n] -> [u](handle, handle) -> *unionfind [n]
  val find [n] : unionfind [n] -> handle -> handle
}

def binary_search [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n - 1)
    while l < r do
      let t = l + (r - l) / 2
      in if x `lte` xs[t]
         then (l, t)
         else (t + 1, r)
  in l

def both f (a, b) = (f a, f b)

def order 't lte (a, b) : (t, t) =
  if a `lte` b then (a, b) else (b, a)

def swap 't (a, b) : (t, t) = (b, a)

module mk_unionfind : unionfind with handle = i64 = {
  type handle = i64

  type unionfind [n] =
    {parents: [n]handle}

  def none : handle = i64.highest

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ({parents = rep none}, hs)

  def inbound (n: i64) (h: handle) : bool =
    0 <= h && h < n

  def find_by_vector [n] (parents: [n]handle) (h: handle) : handle =
    if inbound n h
    then loop h' = h
         while parents[h'] != none do
           parents[h']
    else none

  def find [n] (uf: unionfind [n]) (h: handle) : handle =
    if inbound n h
    then loop h' = h
         while uf.parents[h'] != none do
           uf.parents[h']
    else none

  def normalize_step [m] [n]
                     (is: [m]handle)
                     (parents: *[n]handle)
                     (ps: [m]handle) : (*[n]handle, [m]handle) =
    let f h =
      if parents[h] == none
      then h
      else if parents[parents[h]] == none
      then parents[h]
      else parents[parents[h]]
    let ps' = map f ps
    let new_parents = scatter parents is ps'
    in (new_parents, ps')

  def normalize [m] [n] (parents: *[n]handle) (is: [m]handle) (ps: [m]handle) : *[n]handle =
    let (new_parents, _) =
      loop (parents, ps)
      for _i < 64 - i64.clz m do
        normalize_step is parents ps
    in new_parents

  def left_maximal_union [n] [u]
                         (parents: *[n]handle)
                         (eqs: [u](handle, handle)) : ?[m].(*[n]handle, [m](handle, handle)) =
    let (l, r) = unzip eqs
    let parents' = reduce_by_index parents i64.min none l r
    let (eqs', done) = partition (\(i, p) -> parents'[i] != p) eqs
    let (is, ps) = unzip done
    let parents'' = normalize (copy parents') is ps
    in (parents'', map (both (find {parents = parents''})) eqs')

  def find_equations [n] [u]
                     (parents: [n]handle)
                     (eqs: [u](handle, handle)) : ?[m].[m](handle, handle) =
    map (both (find_by_vector parents)) eqs
    |> filter (\(l, r) -> l != r && l != none && none != r)

  def union [n] [u]
            ({parents}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let eqs' = find_equations (copy parents) eqs |> map (order (<))
    let (ps', eqs'') = left_maximal_union parents eqs'
    let (ps'', _) = left_maximal_union ps' (map swap eqs'' |> filter (\(a, b) -> a != b))
    in {parents = ps''}
}

module mk_unionfind_sequential : unionfind with handle = i64 = {
  type handle = i64

  type unionfind [n] =
    {parents: [n]handle}

  def none : handle = i64.highest

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ({parents = rep none}, hs)

  def find_parent [n] (uf: unionfind [n]) (h: handle) : handle =
    loop i = h
    while uf.parents[i] != none do
      uf.parents[i]

  def find [n] (uf: unionfind [n]) (h: handle) : handle =
    let h' = find_parent uf h
    in if 0 <= h' && h' < n
       then h'
       else none

  def union [n] [u]
            (uf: unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    loop uf' = copy uf
    for (h, h') in eqs do
      let (i, p) = (find_parent uf' h, find_parent uf' h')
      in if i == p
         then uf'
         else uf' with parents = (uf'.parents with [i] = p)
}
