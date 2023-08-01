import "bitset"

module type universe = {
    type t
    val size : i64
    val universe : [size]t
    val == : t -> t -> bool
    module bitset_module : bitset
}

module mk_set (U: universe) = {

    type t = U.t

    def find_index (a : t) : i64 =
        let cmp a b = if b.0 then b else a
        in (reduce_comm cmp (false, -1) (zip (map (U.==a) U.universe) (indices U.universe))).1
    
    def member (a : t) : bool =
        -1 != find_index a
}