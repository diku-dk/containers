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

def swap 't lte (a, b) : (t, t) =
  if a `lte` b then (a, b) else (b, a)

module mk_unionfind : unionfind with handle = i64 = {
  type handle = i64

  type unionfind [n] =
    {parents: [n]handle}

  def none : handle = i64.highest

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ({parents = rep none}, hs)

  def find_root [n] (parents: [n]handle) (h: handle) : handle =
    loop i = h
    while parents[i] != none do
      parents[i]

  def find [n] (uf: unionfind [n]) (h: handle) : handle =
    let h' = find_root uf.parents h
    in if 0 <= h' && h' < n
       then h'
       else none

  def loop_body [n] [u]
                (parents: *[n]handle)
                (eqs: [u](handle, handle)) : ?[m].(*[n]handle, [m](handle, handle)) =
    let (l, r) = unzip eqs
    let parents' = reduce_by_index parents i64.min none l r
    let eqs' =
      map (swap (<=) <-< both (find_root parents')) eqs
      |> zip (indices eqs)
      |> filter (\(j, (i, p)) -> parents'[j] != p && i != p)
      |> map (.1)
    in (parents', copy eqs')

  def union [n] [u]
            ({parents}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let ps = parents
    let (ps', _) =
      loop (ps', eqs') =
             ( ps
             , map (swap (<=) <-< both (find_root ps)) eqs
               |> filter (uncurry (!=))
             )
      while length eqs' != 0 do
        loop_body ps' eqs'
    in {parents = ps'}
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
