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
              (old: [w](i64, i64, [m]i64)) : ( rng
                                             , [](k, i64)
                                             , [](i64, i64, [m]i64)
                                             , [](i64, i64, [m]i64)
                                             ) =
  let (_, old_shape, old_consts) = unzip3 old
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
  let new_keys =
    filter (\(_, o) -> not seg_has_no_collision[o]) old_keys
    |> map (\(k, o) -> (k, new_offsets[o]))
  let (done, temp_not_done) =
    zip seg_has_no_collision old
    |> partition (\(f, _) -> f)
    |> (\(a, b) -> (map (.1) a, map (.1) b))
  let z = length done
  let s = length temp_not_done
  let (rngs, new_not_done_consts) =
    engine.split_rng s old_rng
    |> map (generate_consts m)
    |> unzip
  let not_done =
    map2 (\(i, shp, _) c -> (i, shp + 1, c))
         (temp_not_done :> [s](i64, i64, [m]i64))
         new_not_done_consts
  let new_rng = engine.join_rng rngs
  in ( new_rng
     , new_keys
     , not_done :> [s](i64, i64, [m]i64)
     , done :> [z](i64, i64, [m]i64)
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
  let (final_r, _, _, done) =
    loop ( old_rng
         , old_keys
         , old_not_done
         , old_done
         ) =
           ( r
           , init_keys
           , zip3 (iota s) shape init_level_two_consts
           , []
           )
    while length old_not_done != 0 do
      let ( new_rng
          , new_keys
          , new_not_done
          , new_done
          ) =
        loop_body hash old_rng old_keys old_not_done
      in ( new_rng
         , new_keys
         , new_not_done
         , old_done ++ new_done
         )
  let ( order
      , unordered_level_two_shape
      , unordered_level_two_consts
      ) =
    unzip3 done
  let shape_dest = replicate s 0
  let level_two_shape =
    scatter shape_dest order unordered_level_two_shape
  let consts_dest = replicate s (replicate m 0)
  let level_two_consts =
    scatter consts_dest order unordered_level_two_consts
  in (final_r, level_one_consts, level_two_consts, level_two_shape)

def seed = engine.rng_from_seed [1]
def hashi64 (a: [1]i64) (b: i64) = a[0] * b
