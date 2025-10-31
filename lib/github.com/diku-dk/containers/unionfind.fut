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

def bimap f g (a, b) = (f a, g b)

def order 't lte (a, b) : (t, t) =
  if a `lte` b then (a, b) else (b, a)

def swap 't (a, b) : (t, t) = (b, a)

module mk_unionfind : unionfind with handle = i64 = {
  type handle = i64

  type unionfind [n] =
    { parents: [n]handle
    , sizes: [n]i64
    , temporary_indices: [n]i64
    }

  def none : handle = i64.highest

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ( { parents = rep none
         , sizes = rep 1
         , temporary_indices = rep i64.lowest
         }
       , hs
       )

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

  def normalize [m] [n]
                (parents: *[n]handle)
                (is: [m]handle)
                (ps: [m]handle) : (*[n]handle, [m]handle) =
    let (new_parents, _) =
      loop (parents, ps)
      for _i < 64 - i64.clz m do
        normalize_step is parents ps
    in (new_parents, ps)

  def left_maximal_union [n] [u]
                         (parents: *[n]handle)
                         (sizes: *[n]i64)
                         (temporary_indices: *[n]i64)
                         (eqs: [u](handle, handle)) : ?[m].( *[n]handle
                                                           , *[n]i64
                                                           , *[n]i64
                                                           , [m](handle, handle)
                                                           ) =
    let (lefts, rights) = unzip eqs
    let rights_is = indices rights
    let temporary_indices' =
      reduce_by_index temporary_indices i64.min i64.highest lefts rights_is
    let (done, eqs') =
      zip (indices eqs) eqs
      |> partition (\(i, (_, r)) -> i == temporary_indices'[r])
      |> bimap (map (.1)) (map (.1))
    let (is, ps) = unzip done
    let parents' = scatter parents is ps
    let (new_parents, new_ps) = normalize parents' is ps
    let children_sizes = map (\i -> sizes[i]) is
    let new_sizes = reduce_by_index sizes (+) 0 new_ps children_sizes
    let new_eqs = map (both (find_by_vector new_parents)) eqs'
    let new_temporary_indices = scatter temporary_indices' lefts (rep i64.highest)
    in (new_parents, new_sizes, new_temporary_indices, new_eqs)

  def union [n] [u]
            ({parents, sizes, temporary_indices}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let temp_eqs =
      map ((\(a, b) ->
              if a == none || b == none
              then (a, b)
              else if sizes[a] <= sizes[b]
              then (a, b)
              else (b, a))
           <-< both (find_by_vector parents))
          eqs
      |> filter (\(l, r) -> l != r && l != none && none != r)
    let (parents', sizes', temporary_indices', eqs') =
      left_maximal_union parents sizes temporary_indices temp_eqs
    let temp_eqs' =
      map swap eqs'
      |> filter (\(a, b) -> a != b)
    let (parents'', sizes'', temporary_indices'', _) =
      left_maximal_union parents' sizes' temporary_indices' temp_eqs'
    in { parents = parents''
       , sizes = sizes''
       , temporary_indices = temporary_indices''
       }
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
