import "bitset"

module bitset_u8 = mk_bitset u8

-- ==
-- entry: test_is_empty
-- random input { [100]u8 } output { true }
entry test_is_empty (arr : []u8) : bool =
  all (\c ->
    let c' = i64.u8 c
    let empty_set = bitset_u8.empty c'
    in bitset_u8.is_empty empty_set
  ) arr

-- ==
-- entry: test_complement
-- random input { [100]u8 } output { true }
entry test_complement (arr : []u8) : bool =
  all (\c ->
    let c' = i64.u8 c
    let full_set = bitset_u8.empty c' |> bitset_u8.complement
    in bitset_u8.size full_set == c'
  ) arr

-- ==
-- entry: test_equality
-- random input { [100][20]u8 } output { true }
entry test_equality (arr : [][]u8) : bool =
  all (\arr ->
    let arr' = map i64.u8 arr
    let set = bitset_u8.from_array (i64.u8 u8.highest) arr'
    in set bitset_u8.== set
  ) arr