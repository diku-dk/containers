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

type^ hashset [n] [m] 'k =
  { keys: [n]k
  , offsets: []i64
  , level_one_consts: [m]i64
  , level_one_offsets: []i64
  , level_two_offsets: []i64
  , level_two_consts: [][m]i64
  , level_two_shape: []i64
  , hash: [m]i64 -> k -> i64
  , eq: k -> k -> bool
  }

local
def level_two_hash 'k [m] [w]
                   (hash: [m]i64 -> k -> i64)
                   (level_one_consts: [m]i64)
                   (n: i64)
                   (level_one_offsets: []i64)
                   (level_two_offsets: [w]i64)
                   (level_two_consts: [w][m]i64)
                   (level_two_shape: [w]i64)
                   (key: k) =
  let x = hash level_one_consts key % n
  let o = level_one_offsets[x]
  let cs = level_two_consts[o]
  let s = level_two_shape[o]
  let y = hash cs key % s
  in level_two_offsets[o] + y

def member [n] [m] 'k
           (set: hashset [n] [m] k)
           (key: k) : bool =
  let i =
    level_two_hash set.hash
                   set.level_one_consts
                   n
                   set.level_one_offsets
                   set.level_two_offsets
                   set.level_two_consts
                   set.level_two_shape
                   key
  let key' = set.keys[set.offsets[i]]
  in set.eq key' key

type cmp 'a = #id | #elem a | #collision

local def equiv 'a (eq: a -> a -> bool) (a: cmp a) (b: cmp a): cmp a =
  match (a, b)
  case (_, #id) -> a
  case (#id, _) -> b
  case (_, #collision) -> #collision
  case (#collision, _) -> #collision
  case (#elem a', #elem b') -> if a' `eq` b'
                               then #elem a'
                               else #collision

local def is_not_collision 'a (a: cmp a): bool =
  match a
  case #elem _ -> true
  case #id -> true
  case #collision -> false
  
def loop_body [n] [w] [m] 'k
              (eq: k -> k -> bool)
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
  let has_no_collisions =
    map (\(k, _) -> #elem k) old_keys
    |> hist (equiv eq) #id flat_size is
    |> map is_not_collision
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

def construct [n] [m] 'k
              (r: rng)
              (eq: k -> k -> bool)
              (hash: [m]i64 -> k -> i64)
              (keys: [n]k) =
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
        loop_body eq hash old_rng old_keys old_not_done
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
  let (flat_size, level_two_offsets) = exscan (+) 0 level_two_shape
  let consts_dest = replicate s (replicate m 0)
  let level_two_consts =
    scatter consts_dest order unordered_level_two_consts
  let hash2 =
    level_two_hash hash
                   level_one_consts
                   n
                   level_one_offsets
                   level_two_offsets
                   level_two_consts
                   level_two_shape
  let js = map hash2 keys
  let offsets =
    scatter (replicate flat_size 0) js (iota n)
  in ( final_r
     , { keys = keys
       , offsets = offsets
       , level_one_consts = level_one_consts
       , level_one_offsets = level_one_offsets
       , level_two_offsets = level_two_offsets
       , level_two_consts = level_two_consts
       , level_two_shape = level_two_shape
       , eq = eq
       , hash = hash
       }
     )
