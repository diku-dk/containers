import "maybe"

-- ==
-- entry: test_from_maybe_nothing
-- input { 0i64 } output { 0i64 }
-- input { 1i64 } output { 1i64 }
-- input { 2i64 } output { 2i64 }
-- input { 3i64 } output { 3i64 }
entry test_from_maybe_nothing (c : i64) : i64 =
  let n : maybe i64 = #nothing
  in from_maybe n c

-- ==
-- entry: test_from_maybe_just
-- input { 0i64 } output { 0i64 }
-- input { 1i64 } output { 1i64 }
-- input { 2i64 } output { 2i64 }
-- input { 3i64 } output { 3i64 }
entry test_from_maybe_just (c : i64) : i64 =
  let n : maybe i64 = #just c
  in from_maybe n (-1)

-- ==
-- entry: test_map_maybe_nothing
-- input {  } output { -1i64 }
-- input {  } output { -1i64 }
-- input {  } output { -1i64 }
-- input {  } output { -1i64 }
entry test_map_maybe_nothing : i64 =
  let n : maybe i64 = #nothing
  in map_maybe (*2) n
     |> flip from_maybe (-1)

-- ==
-- entry: test_map_maybe_just
-- input { 0i64 } output { 0i64 }
-- input { 1i64 } output { 2i64 }
-- input { 2i64 } output { 4i64 }
-- input { 3i64 } output { 6i64 }
entry test_map_maybe_just (c : i64) : i64 =
  let n : maybe i64 = #just c
  in map_maybe (*2) n
     |> flip from_maybe (-1)

-- ==
-- entry: test_equal_maybe
-- input { 0i64 } output { true }
-- input { 1i64 } output { true }
-- input { 2i64 } output { true }
-- input { 3i64 } output { true }
entry test_equal_maybe (c : i64) : bool =
  let n : maybe i64 = #just c
  let m : maybe i64 = #nothing
  let eq = equal_maybe (i64.==)
  in eq n n && not (eq n m) && not (eq m n) && eq m m


def combine (a : (i64, i64)) (b : (i64, i64)) : (i64, i64) =
    (a.0, b.1)

def combine' = add_identity combine

-- ==
-- entry: test_add_identity
-- random input { [100]i64 [100]i64 } output { true }
entry test_add_identity (arr : []i64) (arr' : []i64) =
    zip arr arr'
    |> map (\a -> #just)
    |> reduce combine' #nothing
    |> is_just