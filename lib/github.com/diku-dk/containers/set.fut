import "bitset"


-- module mk_set (B: bitset) : set = {
-- 
--     def universe_find_index (a : t) : i64 =
--         let cmp a b = if b.0 then b else a
--         in (reduce_comm cmp (false, -1) (zip (map (U.==a) U.universe) (indices U.universe))).1
--     
--     def universe_member (a : t) : bool =
--         -1 != universe_find_index a
-- }