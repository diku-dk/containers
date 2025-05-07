import "../cpprandom/random"
import "../segmented/segmented"

module engine = xorshift128plus
type rng = xorshift128plus.rng

local
def generate_consts n r =
  let (new_rng, seeds) =
    loop (r', s) = (r, [])
    for _i < n do
      let (r'', s') = engine.rand r'
      in (r'', s ++ [i64.u64 s'])
  in (new_rng, sized n seeds)

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

local
def construct_init_level_two_consts r m s =
  let rngs = engine.split_rng (m * s) r
  let (rngs, init_level_two_consts) = map engine.rand rngs |> unzip
  let r = engine.join_rng rngs
  let init_level_two_consts = map i64.u64 init_level_two_consts |> unflatten
  in (r, init_level_two_consts)

def loop_body [n] [w] [m] 'k
              (hash: [m]i64 -> k -> i64)
              (old_rng: rng)
              (old_keys: [n](k, i64))
              (old_shape: [w]i64)
              (old_consts: [w](i64, [m]i64)) =
  let (flat_size, old_shape_offsets) = exscan (+) 0 old_shape
  let js =
    map (\(k, o) ->
           old_shape_offsets[o] + (hash old_consts[o].1 k % old_shape[o]))
        old_keys
  let has_no_collisions = replicate n 1 |> hist (+) 0i64 flat_size js |> map (<= 1)
  let flag_idxs =
    map2 (\i j -> if i == 0 then -1 else j) old_shape old_shape_offsets
  let flags =
    scatter (replicate flat_size false) flag_idxs (replicate w true)
  let seg_has_no_collision =
    segmented_reduce (&&) true flags has_no_collisions
    |> sized w
  let (_, new_offsets) =
    map2 (\f s -> if f then 0 else s) seg_has_no_collision old_shape
    |> exscan (+) 0
  let new_shape =
    zip seg_has_no_collision old_shape
    |> filter (\(f, _) -> not f)
    |> map (.1)
  let new_keys =
    filter (\(_, o) -> not seg_has_no_collision[o]) old_keys
    |> map (\(k, o) -> (k, new_offsets[o]))
  let (new_done_consts, old_not_done_consts) =
    zip seg_has_no_collision old_consts
    |> partition (\(f, _) -> f)
    |> (\(a, b) -> (map (.1) a, map (.1) b))
  let s = length old_not_done_consts
  let old_not_done_consts = old_not_done_consts :> [s](i64, [m]i64)
  let (rngs, temp_consts) =
    engine.split_rng s old_rng
    |> map (generate_consts m)
    |> unzip
  let new_not_done_consts =
    map2 (\(i, _) cs -> (i, cs)) old_not_done_consts temp_consts
  let new_rng = engine.join_rng rngs
  in (new_rng, new_keys, new_shape, new_not_done_consts, new_done_consts)

def construct [n] [m] 'k (r: rng) (hash: [m]i64 -> k -> i64) (keys: [n]k) =
  let (r, level_one_consts) = generate_consts m r
  let is = map ((% n) <-< hash level_one_consts) keys
  let level_one_counts = replicate n 1 |> hist (+) 0i64 n is
  let level_one_counts_squared = map (** 2) level_one_counts
  let (_, level_one_offsets) =
    map (i64.bool <-< (!= 0)) level_one_counts
    |> exscan (+) 0
  let s = i64.sum level_one_counts_squared
  let (r, init_level_two_consts) =
    construct_init_level_two_consts r s m
    |> (\(a, t) -> (a, zip (iota s) t))
  let shape = filter (!= 0) level_one_counts_squared
  let shape_size = length shape
  let shape = sized shape_size shape
  let init_keys = map2 (\k i -> (k, level_one_offsets[i])) keys is
  let (r, _, _, _, level_two_consts) =
    loop (old_rng, old_keys, old_shape, not_done_consts, done_consts) =
           (r, init_keys, shape, init_level_two_consts, [])
    while length not_done_consts != 0 do
      let ( new_rng
          , new_keys
          , new_shape
          , new_not_done_const
          , new_done_consts
          ) =
        loop_body hash old_rng old_keys old_shape not_done_consts
      in ( new_rng
         , new_keys
         , new_shape
         , new_not_done_const
         , done_consts ++ new_done_consts
         )
  in (r, level_one_consts, level_two_consts)
