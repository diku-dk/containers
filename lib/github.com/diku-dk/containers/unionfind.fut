-- | Module containing different implementations of union-find.
--
-- Union-find is a data structure which support three operation:
-- * `create`@term: Make an empty union-find structure.
-- * `find`@term: Find the representative of an element in the
--   union-find structure.
-- * `union`@term: Make it so to elements in the union-find structure
--   has the same representative.
--
-- The implementations found in this library focuses on bulk parallel
-- find and union operations. There are three different
-- implementations of the parallel union-find data-structure.
-- * `unionfind`@term: A bare bones union-find implementation which
--   has good performance if about O(log n) calls of union is done
--   where `n` is the number of element pairs to be unioned.  If you
--   have linear amount of calls to union then the finds will become
--   expensive and linear in cost.
-- * `union_by_rank`@term: A union-find implementation which uses union
--   by rank leading to the find being O(log n) for a single element
--   where `n` is the number of element pairs that have been unioned.
-- * `union_by_size`@term: A union-find implementation which uses
--   union by size leading to the same properties as
--   `union_by_rank`@term but it has worse performance when
--   benchmarking the union operation.

module type unionfind = {
  -- | A handle is an element in the union-find structure.
  type handle

  -- | The union-find structure.
  type unionfind [n]

  -- | Create an union-find structure of `n`@term handles where
  -- initially every element is not unioned with no other element then
  -- it self.
  val create : (n: i64) -> *unionfind [n]

  -- | Given an array of handles find the representative of each
  -- handle and give back the new union-find structure with the
  -- representatives.
  val find [n] [u] : *unionfind [n] -> [u]handle -> (*unionfind [n], [u]handle)

  -- | Given an array of handles find the representative of each
  -- handle. The unionfind structure will remain unchanged, this may
  -- ruin asymptotic garantees.
  val find' [n] [u] : unionfind [n] -> [u]handle -> [u]handle

  -- | Perform an union between multiple handles, every tuple pair
  -- will be unioned to have the same representative.
  val union [n] [u] : *unionfind [n] -> [u](handle, handle) -> *unionfind [n]

  -- | Retrieve all handles.
  val handles [n] : unionfind [n] -> *[n]handle

  -- | Lookup the handle found at an in bound index in the array
  -- created by the `handles`@term function. If the index is not in
  -- bound an error will be thrown.
  val from_i64 [n] : unionfind [n] -> i64 -> handle

  -- | Lookup the index of a handle found in the array created by the
  -- `handles`@term function. If the handle is not from a union-find
  -- structure that is less than or equal to `n`@term then an error
  -- will be thrown.
  val to_i64 [n] : unionfind [n] -> handle -> i64
}

local
def both f (a, b) = (f a, f b)

local
def bimap f g (a, b) = (f a, g b)

local
def swap 't (a, b) : (t, t) = (b, a)

module unionfind_by_size : unionfind = {
  type handle = i64

  type unionfind [n] =
    { parents: [n]handle
    , sizes: [n]i64
    , temporary_indices: [n]i64
    }

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h <= n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i <= n) i

  def none : handle = i64.highest

  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  def create (n: i64) : *unionfind [n] =
    { parents = rep none
    , sizes = rep 1
    , temporary_indices = rep none
    }

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

  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : [u]handle =
    map (\h ->
           loop h
           while uf.parents[h] != none do
             uf.parents[h])
        hs

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
    let (new_parents, ps) =
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
             if sizes[a] != sizes[b]
             then if sizes[a] < sizes[b] then (a, b) else (b, a)
             else if a < b then (a, b) else (b, a))
          eqs'
    in (new_parents, new_eqs)

  def union [n] [u]
            ({parents, sizes, temporary_indices}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (parents, eqs) = order parents sizes eqs
    let eqs = filter (\(l, r) -> l != r) eqs
    let (new_parents, new_sizes, new_temporary_indices, _) =
      loop (parents, sizes, temporary_indices, eqs)
      while not (null eqs) do
        let (parents, sizes, temporary_indices, eqs) =
          left_maximal_union parents sizes temporary_indices eqs
        let (parents, eqs) = order parents sizes eqs
        let eqs = filter (\(l, r) -> l != r) eqs
        in (parents, sizes, temporary_indices, eqs)
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
    }

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h <= n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i <= n) i

  def none : handle = i64.highest

  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  def create (n: i64) : *unionfind [n] =
    { parents = rep none
    , ranks = rep 0
    }

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
           ({parents, ranks}: *unionfind [n])
           (hs: [u]handle) : (*unionfind [n], [u]handle) =
    let (new_parents, ps) = find_by_vector parents hs
    in ({parents = new_parents, ranks}, ps)

  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : [u]handle =
    map (\h ->
           loop h
           while uf.parents[h] != none do
             uf.parents[h])
        hs

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
    let (new_parents, ps) =
      loop (parents, ps)
      for _i < 64 - i64.clz m do
        normalize_step is parents ps
    in (new_parents, ps)

  def left_maximal_union [n] [u]
                         (parents: *[n]handle)
                         (ranks: *[n]u8)
                         (eqs: [u](handle, handle)) : ?[m].( *[n]handle
                                                           , *[n]u8
                                                           , [m](handle, handle)
                                                           ) =
    let (ls, rs) = unzip eqs
    let parents = reduce_by_index parents i64.min none ls rs
    let (new_eqs, done) =
      copy (partition (\(i, p) -> parents[i] != p) eqs)
    let is = map (.0) done
    let (new_parents, new_ps) = normalize parents is
    let new_ranks_done =
      copy
      <| map2 (\l p ->
                 u8.bool (ranks[l] u8.== ranks[p]) + ranks[p])
              is
              new_ps
    let new_ranks = reduce_by_index ranks u8.max 0 new_ps new_ranks_done
    in (new_parents, new_ranks, new_eqs)

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
             if ranks[a] != ranks[b]
             then if ranks[a] < ranks[b] then (a, b) else (b, a)
             else if a < b then (a, b) else (b, a))
          eqs'
    in (new_parents, new_eqs)

  def union [n] [u]
            ({parents, ranks}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (parents, eqs) = order parents ranks eqs
    let eqs = filter (\(l, r) -> l != r) eqs
    let (new_parents, new_ranks, _) =
      loop (parents, ranks, eqs)
      while not (null eqs) do
        let (parents, ranks, eqs) =
          left_maximal_union parents ranks eqs
        let (parents, eqs) = order parents ranks eqs
        let eqs = filter (\(l, r) -> l != r) eqs
        in (parents, ranks, eqs)
    in { parents = new_parents
       , ranks = new_ranks
       }
}

module unionfind : unionfind = {
  type handle = i64

  type unionfind [n] = {parents: [n]handle}

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h <= n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i <= n) i

  def none : handle = i64.highest

  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  def create (n: i64) : *unionfind [n] =
    {parents = rep none}

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

  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : [u]handle =
    map (\h ->
           loop h
           while uf.parents[h] != none do
             uf.parents[h])
        hs

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
    let invert = true
    let (parents, _, _) =
      loop (parents, eqs, invert)
      while length eqs != 0 do
        let (parents, eqs) = left_maximal_union parents eqs
        let (parents, eqs) = find_eqs_root parents eqs
        let f a = if invert then swap a else id a
        let eqs =
          map (\(a, b) -> if a < b then (a, b) else (b, a)) eqs
          |> map f
          |> filter (\(a, b) -> a != b)
        in (parents, eqs, not invert)
    in {parents}
}
