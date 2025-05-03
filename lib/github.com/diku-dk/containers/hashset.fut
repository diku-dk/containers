import "../cpprandom/random"
import "../segmented/segmented"

module engine = xorshift128plus
type rng = xorshift128plus.rng

local
def generate_seeds r n =
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

type~ hashset [n] [m] 'k =
  { offsets: [n](bool, i64, i64)
  , level_one_hash: [m]i64
  , level_two_hash: [m][]i64
  }

local
def construct_init_level_two_hash r m s =
  let rngs = engine.split_rng (m * s) r
  let (rngs, init_level_two_hash) = map engine.rand rngs |> unzip
  let r = engine.join_rng rngs
  let init_level_two_hash = map i64.u64 init_level_two_hash |> unflatten
  in (r, init_level_two_hash)

local
def create_flags [n] 't
                 (default: t)
                 (flags: [n]t)
                 (shape: [n]i64) : []t =
  let (size, offsets) = exscan (+) 0 shape
  let idxs =
    map2 (\i j -> if i == 0 then -1 else j)
         shape
         offsets
  in scatter (replicate size default) idxs flags

def construct [n] [m] 'k (r: rng) (hash: [m]i64 -> k -> i64) (keys: [n]k) =
  let (r, level_one_hash) = generate_seeds r m
  let is = map ((% n) <-< hash level_one_hash) keys
  let level_one_counts = replicate n 1 |> hist (+) 0i64 n is
  let level_one_counts_squared = map (** 2) level_one_counts
  let level_one_flags = map (!= 0) level_one_counts
  let (s, hash_offset) = map i64.bool level_one_flags |> exscan (+) 0
  let (_, offsets) =
    exscan (+) 0 level_one_counts_squared
    |> (\(l, offs) -> (l, zip3 level_one_flags hash_offset offs))
  let init_keys = map (.2) offsets |> zip3 keys (map (\i -> level_one_counts_squared[i]) is)
  let (r, init_level_two_hash) = construct_init_level_two_hash r m s
  let init_shape = filter (!= 0) level_one_counts_squared
  let (level_two_hash, _, _, r) =
    loop (old_hash, old_keys, old_shape, old_rng) = (init_level_two_hash, init_keys, init_shape, r)
    while length old_keys != 0 do
      let shape_size = length old_shape
      let (flat_size, level_two_offsets) = exscan (+) 0 old_shape
      let js = map (\(k, z, o) -> level_two_offsets[o] + (hash (sized m old_hash[o]) k % z)) old_keys
      let has_no_collisions = replicate n 1 |> hist (+) 0i64 flat_size js |> map (<= 1)
      let flags = create_flags false (replicate shape_size true) (sized shape_size old_shape) |> sized flat_size
      let seg_has_no_collision = segmented_reduce (&&) true flags has_no_collisions
      let shape_is = tabulate shape_size (\i -> if seg_has_no_collision[i] then i else -1)
      let new_shape = scatter old_shape shape_is (replicate shape_size 0)
      let new_keys = filter (\(_, _, o) -> not seg_has_no_collision[o]) old_keys
      let old_rngs = engine.split_rng (shape_size * m) old_rng
      let (js', rngs, vs) =
        map (\i -> zip (replicate m i) (iota m)) (sized (shape_size) (indices seg_has_no_collision))
        |> flatten
        |> map2 (\r' (i, j) ->
                   if seg_has_no_collision[i]
                   then ((-1, -1), r', 0)
                   else let (r'', a) = engine.rand r' in ((i, j), r'', i64.u64 a))
                old_rngs
        |> unzip3
      let new_rng = engine.join_rng rngs
      let new_hash = scatter_2d old_hash js' vs
      in (new_hash, old_keys, new_shape, new_rng)
  in ( { offsets = offsets
       , level_one_hash = level_one_hash
       , level_two_hash = level_two_hash
       }
     , r
     )
