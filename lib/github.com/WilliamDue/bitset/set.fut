import "bitset"


-- module mk_set (B: bitset) : set = {
--     
    -- def universe_find_index (a : t) : i64 =
    --     let cmp a b = if b.0 then b else a
    --     in (reduce_comm cmp (false, -1) (zip (map (U.==a) U.universe) (indices U.universe))).1
    -- 
    -- def universe_look_up ((i, bit_i) : (i64, i32)) : t =
    --     let num_bits = i64.i32 u8.num_bits
    --     let idx = i * num_bits + i64.i32 bit_i
    --     in copy U.universe[idx]
    -- 
    -- def universe_member (a : t) : bool =
    --     -1 != universe_find_index a
    --
-- }