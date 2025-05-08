import "../cpprandom/random"
import "../segmented/segmented"

module engine = xorshift128plus
type rng = xorshift128plus.rng

local
def generate_consts n r =
  engine.split_rng n r
  |> map engine.rand
  |> unzip
  |> (\(a, b) -> (engine.join_rng a, map i64.u64 b))

local
def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) =
  if length as == 0
  then (ne, as)
  else let res = scan op ne as |> rotate (-1)
       let l = copy res[0]
       let res[0] = ne
       in (l, res)

type~ hashset [n] [m] [w] 'k =
  { offsets: [n]i64
  , level_one_consts: [m]i64
  , level_two_consts: [][m]i64
  , keys: [w]k
  }

def loop_body [n] [w] [m] 'k
              (hash: [m]i64 -> k -> i64)
              (old_rng: rng)
              (old_keys: [n](k, i64))
              (old_order: [w]i64)
              (old_shape: [w]i64)
              (old_consts: [w][m]i64) : ?[u][c][t].( rng
                                                   , [u](k, i64)
                                                   , [c]i64
                                                   , [t]i64
                                                   , [c]i64
                                                   , [t]i64
                                                   , [c][m]i64
                                                   , [t][m]i64
                                                   ) =
  let (flat_size, old_shape_offsets) = old_shape |> exscan (+) 0
  let is =
    map (\(k, o) ->
           old_shape_offsets[o] + (hash old_consts[o] k % old_shape[o]))
        old_keys
  let has_no_collisions = replicate n 1 |> hist (+) 0i64 flat_size is |> map (<= 1)
  let flag_idxs =
    map2 (\i j -> if i == 0 then -1 else j) old_shape old_shape_offsets
  let flags =
    scatter (replicate flat_size false) flag_idxs (replicate w true)
  let seg_has_no_collision =
    segmented_reduce (&&) true flags has_no_collisions
    |> sized w
  let (_, new_offsets) =
    map (\f -> if f then 0 else 1) seg_has_no_collision
    |> exscan (+) 0
  let (done_shape, not_done_shape) =
    zip seg_has_no_collision old_shape
    |> partition (\(f, _) -> f)
    |> (\(a, b) -> (map (.1) a, map ((+ 1) <-< (.1)) b))
  let (done_order, not_done_order) =
    zip seg_has_no_collision old_order
    |> partition (\(f, _) -> f)
    |> (\(a, b) -> (map (.1) a, map (.1) b))
  let new_keys =
    filter (\(_, o) -> not seg_has_no_collision[o]) old_keys
    |> map (\(k, o) -> (k, new_offsets[o]))
  let (new_done_consts, old_not_done_consts) =
    zip seg_has_no_collision old_consts
    |> partition (\(f, _) -> f)
    |> (\(a, b) -> (map (.1) a, map (.1) b))
  let z = length new_done_consts
  let s = length old_not_done_consts
  let (rngs, new_not_done_consts) =
    engine.split_rng s old_rng
    |> map (generate_consts m)
    |> unzip
  let new_rng = engine.join_rng rngs
  in ( new_rng
     , new_keys
     , not_done_order :> [s]i64
     , done_order :> [z]i64
     , not_done_shape :> [s]i64
     , done_shape :> [z]i64
     , new_not_done_consts :> [s][m]i64
     , new_done_consts :> [z][m]i64
     )

def construct [n] [m] 'k (r: rng) (hash: [m]i64 -> k -> i64) (keys: [n]k) =
  let (r, level_one_consts) = generate_consts m r
  let is = map ((% n) <-< hash level_one_consts) keys
  let level_one_counts_squared =
    replicate n 1 |> hist (+) 0i64 n is
    |> map (** 2)
  let shape = filter (!= 0) level_one_counts_squared
  let s = length shape
  let shape = sized s shape
  let (_, level_one_offsets) =
    map (i64.bool <-< (!= 0)) level_one_counts_squared
    |> exscan (+) 0
  let (r, init_level_two_consts) =
    engine.split_rng s r
    |> map (generate_consts m)
    |> unzip
    |> (\(rngs, b) -> (engine.join_rng rngs, b))
  let init_keys = map2 (\k i -> (k, level_one_offsets[i])) keys is
  let (r, _, _, order, _, unordered_level_two_shape, _, unordered_level_two_consts) =
    loop ( old_rng
         , old_keys
         , not_done_order
         , done_order
         , not_done_shape
         , done_shape
         , not_done_consts
         , done_consts
         ) =
           ( r
           , init_keys
           , iota s
           , []
           , shape
           , []
           , init_level_two_consts
           , []
           )
    while length not_done_consts != 0 do
      let ( new_rng
          , new_keys
          , new_not_done_order
          , new_done_order
          , new_not_done_shape
          , new_done_shape
          , new_not_done_const
          , new_done_consts
          ) =
        loop_body hash old_rng old_keys not_done_order not_done_shape not_done_consts
      in ( new_rng
         , new_keys
         , new_not_done_order
         , done_order ++ new_done_order
         , new_not_done_shape
         , done_shape ++ new_done_shape
         , new_not_done_const
         , done_consts ++ new_done_consts
         )
  let order = order :> [s]i64
  let unordered_level_two_shape = unordered_level_two_shape :> [s]i64
  let unordered_level_two_consts = unordered_level_two_consts :> [s][m]i64
  let shape_dest = replicate s 0
  let level_two_shape =
    scatter shape_dest order unordered_level_two_shape
  let consts_dest = replicate s (replicate m 0)
  let level_two_consts =
    scatter consts_dest order unordered_level_two_consts
  in (r, level_one_consts, level_two_consts, level_two_shape)

def seed = engine.rng_from_seed [1]
def hashi64 (a: [1]i64) (b: i64) = a[0] * b
