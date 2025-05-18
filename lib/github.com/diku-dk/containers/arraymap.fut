import "../sorts/merge_sort"
import "../segmented/segmented"
import "opt"
import "array"
import "map"
import "ordkey"

-- | A map that uses a sorted array to represent the mapping.
module mk_arraymap (K: ordkey) : map with key = K.key with ctx = K.ctx = {
  type key = K.key
  type~ ctx = K.ctx

  type~ map [n] 'a =
    { ctx: ctx
    , keys: [n]key
    , vals: [n]a
    }

  def neq lte x y = if x `lte` y then !(y `lte` x) else true

  def pack lte xs =
    zip3 (indices xs) xs (rotate (-1) xs)
    |> filter (\(i, x, y) -> i == 0 || neq lte x y)
    |> map (.1)

  def log2 x = 63 - i64.clz x

  def eytzinger_index (n: i64) (i: i64) =
    let lvl = log2 (i + 1)
    let offset = i64.i32 (1 << (log2 n - lvl))
    let k = i64.i32 ((1 << lvl) - 1)
    in offset + (i - k) * offset * 2 - 1

  def eytzinger [n] 't (xs: [n]t) : ?[m].[m]t =
    let m = 2 ** (i64.num_bits - i64.clz n |> i64.i32) - 1
    let dest = if n == 0 then [] else replicate m xs[n - 1]
    let xs' = scatter dest (indices xs) xs
    let f i = xs'[eytzinger_index m i]
    in tabulate m f

  def ffs x = i64.ctz x + 1

  def eytzinger_search [n] 't (eq: t -> t -> bool) (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
    let k =
      loop k = 1
      while k <= n do
        if flip lte (xs[k - 1]) x
        then 2 * k
        else 2 * k + 1
    let i = (k >> i64.i32 (ffs (!k))) - 1
    in if 0 <= i && i < n && eq xs[i] x then i else -1

  def from_array [u] 'v (ctx: ctx) (kvs: [u](key, v)) : ?[n].map [n] v =
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) (\x y -> K.lte ctx x ctx y)
      |> pack (\x y -> K.lte ctx x.0 ctx y.0)
      |> eytzinger
      |> unzip
    in {keys, vals, ctx}

  def from_array_rep [u] 'v (ctx: ctx) (keys: [u]key) (v: v) : ?[n].map [n] v =
    let lte x y = K.lte ctx x ctx y
    let keys =
      keys
      |> merge_sort lte
      |> pack lte
      |> eytzinger
    in {keys, vals = map (const v) keys, ctx}

  def from_array_hist [u] 'v (ctx: ctx) (op: v -> v -> v) (ne: v) (kvs: [u](key, v)) : ?[n].map [n] v =
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) (\x y -> K.lte ctx x ctx y)
      |> (\kvs ->
            let (keys, vals) = unzip kvs
            let flags =
              map3 (\i x y -> i == 0 || K.eq ctx x ctx y)
                   (indices kvs)
                   keys
                   (rotate (-1) keys)
            let [m] (keys_uniq: [m]key) = zip keys flags |> filter (.1) |> map (.0)
            in zip keys_uniq (sized m (segmented_reduce op ne flags vals)))
      |> eytzinger
      |> unzip
    in {keys, vals, ctx}

  def unsafe_from_array [u] 'v (ctx: ctx) (kvs: [u](key, v)) : ?[n].map [n] v =
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) (\x y -> K.lte ctx x ctx y)
      |> eytzinger
      |> unzip
    in {keys, vals, ctx}

  def unsafe_from_array_rep [n] 'v (ctx: ctx) (keys: [n]key) (v: v) : ?[n].map [n] v =
    let keys =
      keys
      |> merge_sort (\x y -> K.lte ctx x ctx y)
      |> eytzinger
    in {keys, vals = map (const v) keys, ctx}

  def unsafe_from_array_hist [u] 'v (ctx: ctx) (op: v -> v -> v) (v: v) (kvs: [u](key, v)) : ?[n].map [n] v =
    from_array_hist ctx op v kvs

  def lookup_index [n] 'a (ctx: ctx) (k: key) (m: map [n] a) : i64 =
    eytzinger_search (\x y -> K.eq m.ctx x ctx y)
                     (\x y -> K.lte m.ctx x ctx y)
                     m.keys
                     k

  def lookup [n] 'a (ctx: ctx) (k: key) (m: map [n] a) : opt a =
    match lookup_index ctx k m
    case -1 -> #none
    case i -> #some m.vals[i]

  def member [n] 'a (ctx: ctx) (k: key) (m: map [n] a) : bool =
    match lookup ctx k m
    case #none -> false
    case #some _ -> true

  def not_member ctx k xs =
    !(member ctx k xs)

  def hist [n] [u] 'v (op: v -> v -> v) (ne: v) (m: map [n] v) (kvs: [u](key, v)) : map [n] v =
    let is = map (\k -> lookup_index m.ctx k m) (map (.0) kvs)
    in m with vals = reduce_by_index (copy m.vals) op ne is (map (.1) kvs)

  def to_array 'a (m: map [] a) =
    zip m.keys m.vals

  def update [n] [u] 'v (m: map [n] v) (kvs: [u](key, v)) : map [n] v =
    let is = map (\k -> lookup_index m.ctx k m) (map (.0) kvs)
    in m with vals = scatter (copy m.vals) is (map (.1) kvs)

  def size [n] 'a (_: map [n] a) = n

  def context [n] 'a (m: map [n] a) = m.ctx

  def insert [n] [u] 'v (ctx: ctx) (m: map [n] v) (kvs: [u](key, v)) : ?[m].map [m] v =
    from_array ctx (to_array m ++ kvs)

  def insert_hist [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (m: map [n] v)
                  (kvs: [u](key, v)) : ?[m].map [m] v =
    from_array_hist ctx op ne (to_array m ++ kvs)

  def map [n] 'a 'b (g: a -> b) ({ctx, keys, vals}: map [n] a) : map [n] b =
    {ctx, keys, vals = map g vals}

  def map_with_key [n] 'a 'b (g: key -> a -> b) ({ctx, keys, vals}: map [n] a) : map [n] b =
    let vals = map2 g keys vals
    in {ctx, keys, vals}
}
