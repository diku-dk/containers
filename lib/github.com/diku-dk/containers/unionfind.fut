import "hashkey"
import "hashmap"
import "opt"

module type unionfind = {
  type handle
  type unionfind [n]
  val (==) : handle -> handle -> bool
  val create : (n: i64) -> (unionfind [n], [n]handle)
  val union [n] [u] : *unionfind [n] -> [u](handle, handle) -> *unionfind [n]
  val find [n] [u] : *unionfind [n] -> [u]handle -> (*unionfind [n], [u]handle)
}

def both f (a, b) = (f a, f b)

def bimap f g (a, b) = (f a, g b)

def swap 't (a, b) : (t, t) = (b, a)

module unionfind_by_size : unionfind = {
  type handle = i64

  type unionfind [n] =
    { parents: [n]handle
    , sizes: [n]i64
    , temporary_indices: [n]i64
    }

  def (==) (a: handle) (b: handle) = a == b

  def none : handle = i64.highest

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ( { parents = rep none
         , sizes = rep 1
         , temporary_indices = rep none
         }
       , hs
       )

  def find_by_vector [n] [u]
                     (parents: *[n]handle)
                     (hs: [u]handle) : (*[n]handle, [u]handle) =
    ( parents
    , map (\h ->
             loop h
             while parents[h] != none do
               parents[h])
          hs
    )

  def find [n] [u]
           ({parents, sizes, temporary_indices}: *unionfind [n])
           (hs: [u]handle) : (*unionfind [n], [u]handle) =
    let (new_parents, ps) = find_by_vector parents hs
    in ({parents = new_parents, sizes, temporary_indices}, ps)

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
                (is: [m]handle) : (*[n]handle, [m]handle) =
    let ps = is
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
    let lefts = map (.0) eqs
    let eq_is = indices eqs
    let temporary_indices =
      reduce_by_index temporary_indices i64.min i64.highest lefts eq_is
    let (done, eqs) =
      zip (indices eqs) eqs
      |> partition (\(i, (l, _)) -> i == temporary_indices[l])
      |> bimap (map (.1)) (map (.1))
    let (is, ps) = unzip done
    let parents = scatter parents is ps
    let (new_parents, new_ps) = normalize parents is
    let children_sizes = map (\i -> sizes[i]) is
    let new_sizes = reduce_by_index sizes (+) 0 new_ps children_sizes
    let new_eqs = copy eqs
    let new_temporary_indices = scatter temporary_indices lefts (rep i64.highest)
    in (new_parents, new_sizes, new_temporary_indices, new_eqs)

  def order [n] [u]
            (parents: *[n]handle)
            (sizes: [n]i64)
            (eqs: [u](handle, handle)) : ( *[n]handle
                                         , [u](handle, handle)
                                         ) =
    let eqs_elems = unzip eqs |> uncurry (++)
    let (new_parents, new_eqs_elems) = find_by_vector parents eqs_elems
    let eqs' = split new_eqs_elems |> uncurry zip
    let new_eqs =
      map (\(a, b) ->
             if a == none || b == none
             then (a, b)
             else if sizes[a] != sizes[b]
             then if sizes[a] < sizes[b] then (a, b) else (b, a)
             else if a < b then (a, b) else (b, a))
          eqs'
    in (new_parents, new_eqs)

  def union [n] [u]
            ({parents, sizes, temporary_indices}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (parents, eqs) = order parents sizes eqs
    let eqs = filter (\(l, r) -> l != r && l != none && none != r) eqs
    let (new_parents, new_sizes, new_temporary_indices, _) =
      loop (parents, sizes, temporary_indices, eqs)
      while not (null eqs) do
        let (parents', sizes', temporary_indices', eqs') =
          left_maximal_union parents sizes temporary_indices eqs
        let (parents', eqs') = order parents' sizes' eqs'
        let eqs' = filter (\(l, r) -> l != r) eqs'
        in (parents', sizes', temporary_indices', eqs')
    in { parents = new_parents
       , sizes = new_sizes
       , temporary_indices = new_temporary_indices
       }
}

module unionfind_by_rank : unionfind = {
  type handle = i64

  type unionfind [n] =
    { parents: [n]handle
    , ranks: [n]u8
    , temporary_indices: [n]i64
    , is_same_rank: [n]bool
    }

  def (==) (a: handle) (b: handle) = a == b

  def none : handle = i64.highest

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ( { parents = rep none
         , ranks = rep 0
         , temporary_indices = rep none
         , is_same_rank = rep false
         }
       , hs
       )

  def find_by_vector [n] [u]
                     (parents: *[n]handle)
                     (hs: [u]handle) : (*[n]handle, [u]handle) =
    ( parents
    , map (\h ->
             loop h
             while parents[h] != none do
               parents[h])
          hs
    )

  def find [n] [u]
           ({parents, ranks, temporary_indices, is_same_rank}: *unionfind [n])
           (hs: [u]handle) : (*unionfind [n], [u]handle) =
    let (new_parents, ps) = find_by_vector parents hs
    in ({parents = new_parents, ranks, temporary_indices, is_same_rank}, ps)

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
                (is: [m]handle) : (*[n]handle, [m]handle) =
    let ps = is
    let (new_parents, _) =
      loop (parents, ps)
      for _i < 64 - i64.clz m do
        normalize_step is parents ps
    in (new_parents, ps)

  def left_maximal_union [n] [u]
                         (parents: *[n]handle)
                         (ranks: *[n]u8)
                         (temporary_indices: *[n]i64)
                         (is_same_rank: *[n]bool)
                         (eqs: [u](handle, handle)) : ?[m].( *[n]handle
                                                           , *[n]u8
                                                           , *[n]i64
                                                           , *[n]bool
                                                           , [m](handle, handle)
                                                           ) =
    let lefts = map (.0) eqs
    let eq_is = indices eqs
    let temporary_indices =
      reduce_by_index temporary_indices i64.min i64.highest lefts eq_is
    let (done, eqs) =
      zip (indices eqs) eqs
      |> partition (\(i, (l, _)) -> i == temporary_indices[l])
      |> bimap (map (.1)) (map (.1))
    let (is, ps) = unzip done
    let parents = scatter parents is ps
    let (new_parents, new_ps) = normalize parents is
    let is_same_rank_done = map (\(l, r) -> ranks[l] u8.== ranks[r]) done
    let is_same_rank =
      reduce_by_index is_same_rank (||) false new_ps is_same_rank_done
    let new_ranks_done =
      map (\p -> u8.bool is_same_rank[p] + ranks[p]) new_ps
    let new_ranks = scatter ranks new_ps new_ranks_done
    let new_is_same_rank = scatter is_same_rank new_ps (rep false)
    let new_eqs = copy eqs
    let new_temporary_indices = scatter temporary_indices lefts (rep i64.highest)
    in (new_parents, new_ranks, new_temporary_indices, new_is_same_rank, new_eqs)

  def order [n] [u]
            (parents: *[n]handle)
            (ranks: [n]u8)
            (eqs: [u](handle, handle)) : ( *[n]handle
                                         , [u](handle, handle)
                                         ) =
    let eqs_elems = unzip eqs |> uncurry (++)
    let (new_parents, new_eqs_elems) = find_by_vector parents eqs_elems
    let eqs' = split new_eqs_elems |> uncurry zip
    let new_eqs =
      map (\(a, b) ->
             if a == none || b == none
             then (a, b)
             else if ranks[a] != ranks[b]
             then if ranks[a] < ranks[b] then (a, b) else (b, a)
             else if a < b then (a, b) else (b, a))
          eqs'
    in (new_parents, new_eqs)

  def union [n] [u]
            ({parents, ranks, temporary_indices, is_same_rank}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (parents, eqs) = order parents ranks eqs
    let eqs = filter (\(l, r) -> l != r && l != none && none != r) eqs
    let (new_parents, new_ranks, new_temporary_indices, new_is_same_rank, _) =
      loop (parents, ranks, temporary_indices, is_same_rank, eqs)
      while not (null eqs) do
        let (parents', ranks', temporary_indices', is_same_rank', eqs') =
          left_maximal_union parents ranks temporary_indices is_same_rank eqs
        let (parents', eqs') = order parents' ranks' eqs'
        let eqs' = filter (\(l, r) -> l != r) eqs'
        in (parents', ranks', temporary_indices', is_same_rank', eqs')
    in { parents = new_parents
       , ranks = new_ranks
       , temporary_indices = new_temporary_indices
       , is_same_rank = new_is_same_rank
       }
}

module unionfind : unionfind = {
  type handle = i64

  type unionfind [n] = {parents: [n]handle}

  def (==) (a: handle) (b: handle) = a == b

  def none : handle = i64.highest

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ({parents = rep none}, hs)

  def find_by_vector [n] [u]
                     (parents: *[n]handle)
                     (hs: [u]handle) : (*[n]handle, [u]handle) =
    let ps =
      map (\h ->
             loop h
             while parents[h] != none do
               parents[h])
          hs
    in (parents, ps)

  def find [n] [u]
           ({parents}: *unionfind [n])
           (hs: [u]handle) : (*unionfind [n], [u]handle) =
    let (new_parents, ps) = find_by_vector parents hs
    in ({parents = new_parents}, ps)

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
                (is: [m]handle) : *[n]handle =
    let ps = is
    let (parents, _) =
      loop (parents, ps)
      for _i < 64 - i64.clz m do
        normalize_step is parents ps
    in parents

  def find_eqs_root [n] [u]
                    (parents: *[n]handle)
                    (eqs: [u](handle, handle)) : ( *[n]handle
                                                 , [u](handle, handle)
                                                 ) =
    let eqs_elems = unzip eqs |> uncurry (++)
    let (new_parents, new_eqs_elems) = find_by_vector parents eqs_elems
    let eqs = split new_eqs_elems |> uncurry zip
    in (new_parents, eqs)

  def left_maximal_union [n] [u]
                         (parents: *[n]handle)
                         (eqs: [u](handle, handle)) : ?[m].(*[n]handle, [m](handle, handle)) =
    let (l, r) = unzip eqs
    let parents = reduce_by_index parents i64.min none l r
    let (eqs, done) =
      copy (partition (\(i, p) -> parents[i] != p) eqs)
    let parents = normalize parents (map (.0) done)
    in (parents, eqs)

  def union [n] [u]
            ({parents}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (parents, eqs) = find_eqs_root parents eqs
    let eqs = map (\(a, b) -> if a < b then (a, b) else (b, a)) eqs
    let eqs = filter (\(a, b) -> a != b) eqs
    let (parents, eqs) = left_maximal_union parents eqs
    let (parents, eqs) = find_eqs_root parents eqs
    let eqs = map swap eqs |> filter (\(a, b) -> a != b)
    let (parents, _) = left_maximal_union parents eqs
    in {parents}
}

module unionfind_sequential : unionfind = {
  type handle = i64

  type unionfind [n] =
    {parents: [n]handle}

  def none : handle = i64.highest

  def (==) (a: handle) (b: handle) = a == b

  def create (n: i64) : (unionfind [n], [n]handle) =
    let hs = iota n
    in ({parents = rep none}, hs)

  def find_one [n] (uf: unionfind [n]) h =
    loop h while uf.parents[h] != none do
      uf.parents[h]

  def find [n] [u]
           (uf: *unionfind [n])
           (hs: [u]handle) : (*unionfind [n], [u]handle) =
    let hs' = map (find_one uf) hs
    in (uf, hs')

  def union [n] [u]
            (uf: unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    loop uf' = copy uf
    for (h, h') in eqs do
      let (i, p) = (find_one uf' h, find_one uf' h')
      in if i == p
         then uf'
         else uf' with parents = (uf'.parents with [i] = p)
}
