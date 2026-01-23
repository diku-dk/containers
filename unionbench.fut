import "lib/github.com/diku-dk/containers/unionfind"

module unionfind_sequential_work_efficient : unionfind = {
  type handle = i64

  type unionfind [n] =
    { parents: [n]handle
    , ranks: [n]u8
    }

  def none : handle = i64.highest

  #[sequential]
  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h <= n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i <= n) i

  #[sequential]
  def create (n: i64) : *unionfind [n] =
    {parents = rep none, ranks = rep 0}

  #[sequential]
  def find_one [n] (parents: *[n]handle) (h: handle) : (*[n]handle, handle) =
    loop (parents, h) while parents[h] != none do
      let parents[h] = if parents[parents[h]] != none then parents[parents[h]] else parents[h]
      in (parents, parents[h])

  #[sequential]
  def find [n] [u]
           ({parents, ranks}: *unionfind [n])
           (hs: [u]handle) : (*unionfind [n], [u]handle) =
    let hs' = copy hs
    let (parents, hs) =
      loop (parents, hs') for (i, h) in zip (indices hs) hs do
        let (parents, h) = find_one parents h
        let hs'[i] = h
        in (parents, hs')
    in ({parents, ranks}, hs)

  #[sequential]
  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : *[u]handle =
    map (\h ->
           loop h
           while uf.parents[h] != none do
             uf.parents[h])
        hs

  #[sequential]
  def union [n] [u]
            ({parents, ranks}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (parents, ranks) =
      loop (parents, ranks)
      for (h, h') in eqs do
        let (parents, i) = find_one parents h
        let (parents, p) = find_one parents h'
        in if i == p
           then (parents, ranks)
           else let (i, p) = if ranks[i] <= ranks[p] then (i, p) else (p, i)
                let ranks =
                  if ranks[i] u8.== ranks[p]
                  then ranks with [p] = ranks[p] + 1
                  else ranks
                in (parents with [i] = p, ranks)
    in {parents, ranks}
}

module type bench = {
  type handle
  type unionfind [n]
  val random [n] [m] : (unionfind [n], [m](handle, handle))
  val linear [n] [m] : (unionfind [n], [m](handle, handle))
  val single [n] [m] : (unionfind [n], [m](handle, handle))
  val inverse_single [n] [m] : (unionfind [n], [m](handle, handle))
  val all [n] [m] : *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val halving [n] [m] : *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val reverse_halving [n] [m] : *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val chunked [n] [m] : i64 -> *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val find [n] [m] : (t: i64) -> *unionfind [n] -> [m]handle -> [t * m]handle

  val infer :
    (n: i64)
    -> (m: i64)
    -> (data: (unionfind [n], [m](handle, handle)))
    -> (unionfind [n], [m](handle, handle))

  val compose :
    (n: i64)
    -> (m: i64)
    -> (data: (unionfind [n], [m](handle, handle)))
    -> (insert: *unionfind [n] -> [m](handle, handle) -> *unionfind [n])
    -> (unionfind [n], [n]handle)
}

module mk_bench (U: unionfind)
  : bench
    with handle = U.handle
    with unionfind [n] = U.unionfind [n] = {
  type handle = U.handle
  type unionfind [n] = U.unionfind [n]

  -- | Multiply-shift hash function https://arxiv.org/abs/1504.06804
  #[inline]
  def hash (a: (u64, u64)) (b: (u64, u64)) (x: u64) : u64 =
    let y_mul_lo = a.0 * x
    in u64.mul_hi a.1 x + b.1 + u64.bool (y_mul_lo < b.0)

  #[inline]
  def rand (n: i64) : [n]i64 =
    let seed = (n + 2677) * 27644437
    let a0 = 0x8422d1795f837e8b
    let a1 = 0x78f81f96f93e6ca9
    let b0 = 0x643518302a112aa1
    let b1 = 0x58fe49a7f968fbe7
    in iota n
       |> map (i64.u64
               <-< hash (a0, a1) (b0, b1)
               <-< u64.i64
               <-< (+ seed))

  def equations (n: i64) (m: i64) : [m](i64, i64) =
    rand (m + m)
    |> map (% n)
    |> split
    |> uncurry zip

  def random [n] [m] : (unionfind [n], [m](handle, handle)) =
    let uf = U.create n
    let hs = U.handles uf
    let eqs =
      equations n m
      |> map (\(i, j) -> (hs[i], hs[j]))
    in (uf, eqs)

  def single [n] [m] : (unionfind [n], [m](handle, handle)) =
    let uf = U.create n
    let hs = U.handles uf
    let hs' = take m hs
    in (uf, map (\h -> (hs'[0], h)) hs')

  def inverse_single [n] [m] : (unionfind [n], [m](handle, handle)) =
    let uf = U.create n
    let hs = U.handles uf
    let hs' = take m hs
    in (uf, map (\h -> (h, hs'[0])) hs')

  def linear [n] [m] : (unionfind [n], [m](handle, handle)) =
    let uf = U.create n
    let hs = U.handles uf
    let hs' = take m hs
    in (uf, zip hs' (rotate 1 hs'))

  def all [n] [m] (uf: *unionfind [n]) (hs: [m](handle, handle)) : *unionfind [n] =
    U.union uf hs

  def halving [n] [m] (uf: *unionfind [n]) (hs: [m](handle, handle)) : *unionfind [n] =
    let (uf', _) =
      loop (uf, hs) while length hs != 0 do
        let a = 1 + (length hs - 1) / 2
        let b = length hs / 2
        let (xs, ys) = sized (a + b) hs |> split
        let uf = U.union uf xs
        in (uf, ys)
    in uf'

  def reverse_halving [n] [m] (uf: *unionfind [n]) (hs: [m](handle, handle)) : *unionfind [n] =
    let (uf, _, _) =
      loop (uf, hs, a) = (uf, hs, 1)
      while length hs != 0 do
        let k = length hs
        let b = i64.max 0 (k - a)
        let (xs, ys) = sized (i64.min k a + b) hs |> split
        let uf = U.union uf xs
        in (uf, ys, 2 * a)
    in uf

  def chunked [n] [m] (step: i64) (uf: *unionfind [n]) (hs: [m](handle, handle)) : *unionfind [n] =
    let (uf, _) =
      loop (uf, hs) = (uf, hs)
      while length hs != 0 do
        let k = length hs
        let b = i64.max 0 (k - step)
        let (xs, ys) = sized (i64.min k step + b) hs |> split
        let uf = U.union uf xs
        in (uf, ys)
    in uf

  def find [n] [m] (steps: i64) (uf: *unionfind [n]) (hs: [m]handle) =
    let hs' = replicate steps hs |> flatten
    let (_, res) = U.find uf hs'
    in res

  def infer (n: i64)
            (m: i64)
            (data: (unionfind [n], [m](handle, handle))) : (unionfind [n], [m](handle, handle)) =
    copy data

  def compose (n: i64)
              (m: i64)
              (data: (unionfind [n], [m](handle, handle)))
              (insert: *unionfind [n] -> [m](handle, handle) -> *unionfind [n]) : (unionfind [n], [n]handle) =
    let (uf, eqs) = copy data
    let uf = insert uf eqs
    in (uf, U.handles uf)
}

def chunk_size = 10000i64

module bench_unionfind = mk_bench unionfind

entry unionfind_random n m =
  bench_unionfind.infer n m bench_unionfind.random

entry unionfind_linear n m =
  bench_unionfind.infer n m bench_unionfind.linear

entry unionfind_single n m =
  bench_unionfind.infer n m bench_unionfind.single

entry unionfind_inverse_single n m =
  bench_unionfind.infer n m bench_unionfind.inverse_single

entry unionfind_all_random n m =
  bench_unionfind.compose n m bench_unionfind.random bench_unionfind.all

entry unionfind_all_linear n m =
  bench_unionfind.compose n m bench_unionfind.linear bench_unionfind.all

entry unionfind_all_single n m =
  bench_unionfind.compose n m bench_unionfind.single bench_unionfind.all

entry unionfind_all_inverse_single n m =
  bench_unionfind.compose n m bench_unionfind.inverse_single bench_unionfind.all

entry unionfind_halving_random n m =
  bench_unionfind.compose n m bench_unionfind.random bench_unionfind.halving

entry unionfind_halving_linear n m =
  bench_unionfind.compose n m bench_unionfind.linear bench_unionfind.halving

entry unionfind_halving_single n m =
  bench_unionfind.compose n m bench_unionfind.single bench_unionfind.halving

entry unionfind_halving_inverse_single n m =
  bench_unionfind.compose n m bench_unionfind.inverse_single bench_unionfind.halving

entry unionfind_reverse_halving_random n m =
  bench_unionfind.compose n m bench_unionfind.random bench_unionfind.reverse_halving

entry unionfind_reverse_halving_linear n m =
  bench_unionfind.compose n m bench_unionfind.linear bench_unionfind.reverse_halving

entry unionfind_reverse_halving_single n m =
  bench_unionfind.compose n m bench_unionfind.single bench_unionfind.reverse_halving

entry unionfind_reverse_halving_inverse_single n m =
  bench_unionfind.compose n m bench_unionfind.inverse_single bench_unionfind.reverse_halving

entry unionfind_chunked_random n m =
  bench_unionfind.compose n m bench_unionfind.random (bench_unionfind.chunked chunk_size)

entry unionfind_chunked_linear n m =
  bench_unionfind.compose n m bench_unionfind.linear (bench_unionfind.chunked chunk_size)

entry unionfind_chunked_single n m =
  bench_unionfind.compose n m bench_unionfind.single (bench_unionfind.chunked chunk_size)

entry unionfind_chunked_inverse_single n m =
  bench_unionfind.compose n m bench_unionfind.inverse_single (bench_unionfind.chunked chunk_size)

-- ==
-- entry: unionfind_all_bench unionfind_halving_bench unionfind_reverse_halving_bench unionfind_chunked_bench
-- compiled script input { unionfind_random 1000000i64 200000i64 }
-- compiled script input { unionfind_linear 1000000i64 200000i64 }
-- compiled script input { unionfind_single 1000000i64 200000i64 }
-- compiled script input { unionfind_inverse_single 1000000i64 200000i64 }
entry unionfind_all_bench = bench_unionfind.all
entry unionfind_halving_bench = bench_unionfind.halving
entry unionfind_reverse_halving_bench = bench_unionfind.reverse_halving
entry unionfind_chunked_bench = bench_unionfind.chunked chunk_size

-- ==
-- entry: unionfind_find
-- compiled script input { unionfind_all_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_all_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_all_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_all_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_halving_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_halving_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_halving_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_halving_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_reverse_halving_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_reverse_halving_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_reverse_halving_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_reverse_halving_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_chunked_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_chunked_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_chunked_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_chunked_inverse_single 10000000i64 10000000i64 }
entry unionfind_find = bench_unionfind.find 10

module bench_unionfind_by_size = mk_bench unionfind_by_size

entry unionfind_by_size_random n m =
  bench_unionfind_by_size.infer n m bench_unionfind_by_size.random

entry unionfind_by_size_linear n m =
  bench_unionfind_by_size.infer n m bench_unionfind_by_size.linear

entry unionfind_by_size_single n m =
  bench_unionfind_by_size.infer n m bench_unionfind_by_size.single

entry unionfind_by_size_inverse_single n m =
  bench_unionfind_by_size.infer n m bench_unionfind_by_size.inverse_single

entry unionfind_by_size_all_random n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.random bench_unionfind_by_size.all

entry unionfind_by_size_all_linear n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.linear bench_unionfind_by_size.all

entry unionfind_by_size_all_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.single bench_unionfind_by_size.all

entry unionfind_by_size_all_inverse_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.inverse_single bench_unionfind_by_size.all

entry unionfind_by_size_halving_random n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.random bench_unionfind_by_size.halving

entry unionfind_by_size_halving_linear n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.linear bench_unionfind_by_size.halving

entry unionfind_by_size_halving_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.single bench_unionfind_by_size.halving

entry unionfind_by_size_halving_inverse_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.inverse_single bench_unionfind_by_size.halving

entry unionfind_by_size_reverse_halving_random n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.random bench_unionfind_by_size.reverse_halving

entry unionfind_by_size_reverse_halving_linear n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.linear bench_unionfind_by_size.reverse_halving

entry unionfind_by_size_reverse_halving_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.single bench_unionfind_by_size.reverse_halving

entry unionfind_by_size_reverse_halving_inverse_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.inverse_single bench_unionfind_by_size.reverse_halving

entry unionfind_by_size_chunked_random n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.random (bench_unionfind_by_size.chunked chunk_size)

entry unionfind_by_size_chunked_linear n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.linear (bench_unionfind_by_size.chunked chunk_size)

entry unionfind_by_size_chunked_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.single (bench_unionfind_by_size.chunked chunk_size)

entry unionfind_by_size_chunked_inverse_single n m =
  bench_unionfind_by_size.compose n m bench_unionfind_by_size.inverse_single (bench_unionfind_by_size.chunked chunk_size)

-- ==
-- entry: unionfind_by_size_all_bench unionfind_by_size_halving_bench unionfind_by_size_reverse_halving_bench unionfind_by_size_chunked_bench
-- compiled script input { unionfind_by_size_random 1000000i64 200000i64 }
-- compiled script input { unionfind_by_size_linear 1000000i64 200000i64 }
-- compiled script input { unionfind_by_size_single 1000000i64 200000i64 }
-- compiled script input { unionfind_by_size_inverse_single 1000000i64 200000i64 }
entry unionfind_by_size_all_bench = bench_unionfind_by_size.all
entry unionfind_by_size_halving_bench = bench_unionfind_by_size.halving
entry unionfind_by_size_reverse_halving_bench = bench_unionfind_by_size.reverse_halving
entry unionfind_by_size_chunked_bench = bench_unionfind_by_size.chunked chunk_size

-- ==
-- entry: unionfind_by_size_find
-- compiled script input { unionfind_by_size_all_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_all_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_all_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_all_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_halving_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_halving_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_halving_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_halving_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_reverse_halving_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_reverse_halving_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_reverse_halving_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_reverse_halving_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_chunked_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_chunked_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_chunked_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_size_chunked_inverse_single 10000000i64 10000000i64 }
entry unionfind_by_size_find = bench_unionfind_by_size.find 10

module bench_unionfind_by_rank = mk_bench unionfind_by_rank

entry unionfind_by_rank_random n m =
  bench_unionfind_by_rank.infer n m bench_unionfind_by_rank.random

entry unionfind_by_rank_linear n m =
  bench_unionfind_by_rank.infer n m bench_unionfind_by_rank.linear

entry unionfind_by_rank_single n m =
  bench_unionfind_by_rank.infer n m bench_unionfind_by_rank.single

entry unionfind_by_rank_inverse_single n m =
  bench_unionfind_by_rank.infer n m bench_unionfind_by_rank.inverse_single

entry unionfind_by_rank_all_random n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.random bench_unionfind_by_rank.all

entry unionfind_by_rank_all_linear n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.linear bench_unionfind_by_rank.all

entry unionfind_by_rank_all_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.single bench_unionfind_by_rank.all

entry unionfind_by_rank_all_inverse_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.inverse_single bench_unionfind_by_rank.all

entry unionfind_by_rank_halving_random n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.random bench_unionfind_by_rank.halving

entry unionfind_by_rank_halving_linear n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.linear bench_unionfind_by_rank.halving

entry unionfind_by_rank_halving_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.single bench_unionfind_by_rank.halving

entry unionfind_by_rank_halving_inverse_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.inverse_single bench_unionfind_by_rank.halving

entry unionfind_by_rank_reverse_halving_random n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.random bench_unionfind_by_rank.reverse_halving

entry unionfind_by_rank_reverse_halving_linear n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.linear bench_unionfind_by_rank.reverse_halving

entry unionfind_by_rank_reverse_halving_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.single bench_unionfind_by_rank.reverse_halving

entry unionfind_by_rank_reverse_halving_inverse_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.inverse_single bench_unionfind_by_rank.reverse_halving

entry unionfind_by_rank_chunked_random n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.random (bench_unionfind_by_rank.chunked chunk_size)

entry unionfind_by_rank_chunked_linear n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.linear (bench_unionfind_by_rank.chunked chunk_size)

entry unionfind_by_rank_chunked_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.single (bench_unionfind_by_rank.chunked chunk_size)

entry unionfind_by_rank_chunked_inverse_single n m =
  bench_unionfind_by_rank.compose n m bench_unionfind_by_rank.inverse_single (bench_unionfind_by_rank.chunked chunk_size)

-- ==
-- entry: unionfind_by_rank_all_bench unionfind_by_rank_halving_bench unionfind_by_rank_reverse_halving_bench unionfind_by_rank_chunked_bench
-- compiled script input { unionfind_by_rank_random 1000000i64 200000i64 }
-- compiled script input { unionfind_by_rank_linear 1000000i64 200000i64 }
-- compiled script input { unionfind_by_rank_single 1000000i64 200000i64 }
-- compiled script input { unionfind_by_rank_inverse_single 1000000i64 200000i64 }
entry unionfind_by_rank_all_bench = bench_unionfind_by_rank.all
entry unionfind_by_rank_halving_bench = bench_unionfind_by_rank.halving
entry unionfind_by_rank_reverse_halving_bench = bench_unionfind_by_rank.reverse_halving
entry unionfind_by_rank_chunked_bench = bench_unionfind_by_rank.chunked chunk_size

-- ==
-- entry: unionfind_by_rank_find
-- compiled script input { unionfind_by_rank_all_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_all_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_all_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_all_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_halving_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_halving_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_halving_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_halving_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_reverse_halving_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_reverse_halving_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_reverse_halving_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_reverse_halving_inverse_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_chunked_random 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_chunked_linear 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_chunked_single 10000000i64 10000000i64 }
-- compiled script input { unionfind_by_rank_chunked_inverse_single 10000000i64 10000000i64 }
entry unionfind_by_rank_find = bench_unionfind_by_rank.find 10

module bench_unionfind_sequential_work_efficient = mk_bench unionfind_sequential_work_efficient

entry unionfind_sequential_work_efficient_random n m =
  bench_unionfind_sequential_work_efficient.infer n m bench_unionfind_sequential_work_efficient.random

entry unionfind_sequential_work_efficient_linear n m =
  bench_unionfind_sequential_work_efficient.infer n m bench_unionfind_sequential_work_efficient.linear

entry unionfind_sequential_work_efficient_single n m =
  bench_unionfind_sequential_work_efficient.infer n m bench_unionfind_sequential_work_efficient.single

entry unionfind_sequential_work_efficient_inverse_single n m =
  bench_unionfind_sequential_work_efficient.infer n m bench_unionfind_sequential_work_efficient.inverse_single

entry unionfind_sequential_work_efficient_all_random n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.random bench_unionfind_sequential_work_efficient.all

entry unionfind_sequential_work_efficient_all_linear n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.linear bench_unionfind_sequential_work_efficient.all

entry unionfind_sequential_work_efficient_all_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.single bench_unionfind_sequential_work_efficient.all

entry unionfind_sequential_work_efficient_all_inverse_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.inverse_single bench_unionfind_sequential_work_efficient.all

entry unionfind_sequential_work_efficient_halving_random n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.random bench_unionfind_sequential_work_efficient.halving

entry unionfind_sequential_work_efficient_halving_linear n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.linear bench_unionfind_sequential_work_efficient.halving

entry unionfind_sequential_work_efficient_halving_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.single bench_unionfind_sequential_work_efficient.halving

entry unionfind_sequential_work_efficient_halving_inverse_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.inverse_single bench_unionfind_sequential_work_efficient.halving

entry unionfind_sequential_work_efficient_reverse_halving_random n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.random bench_unionfind_sequential_work_efficient.reverse_halving

entry unionfind_sequential_work_efficient_reverse_halving_linear n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.linear bench_unionfind_sequential_work_efficient.reverse_halving

entry unionfind_sequential_work_efficient_reverse_halving_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.single bench_unionfind_sequential_work_efficient.reverse_halving

entry unionfind_sequential_work_efficient_reverse_halving_inverse_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.inverse_single bench_unionfind_sequential_work_efficient.reverse_halving

entry unionfind_sequential_work_efficient_chunked_random n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.random (bench_unionfind_sequential_work_efficient.chunked chunk_size)

entry unionfind_sequential_work_efficient_chunked_linear n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.linear (bench_unionfind_sequential_work_efficient.chunked chunk_size)

entry unionfind_sequential_work_efficient_chunked_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.single (bench_unionfind_sequential_work_efficient.chunked chunk_size)

entry unionfind_sequential_work_efficient_chunked_inverse_single n m =
  bench_unionfind_sequential_work_efficient.compose n m bench_unionfind_sequential_work_efficient.inverse_single (bench_unionfind_sequential_work_efficient.chunked chunk_size)

-- ==
-- entry: unionfind_sequential_work_efficient_all_bench unionfind_sequential_work_efficient_halving_bench unionfind_sequential_work_efficient_reverse_halving_bench unionfind_sequential_work_efficient_chunked_bench
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_random 100000i64 20000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_linear 100000i64 20000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_single 100000i64 20000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_inverse_single 100000i64 20000i64 }
entry unionfind_sequential_work_efficient_all_bench = bench_unionfind_sequential_work_efficient.all
entry unionfind_sequential_work_efficient_halving_bench = bench_unionfind_sequential_work_efficient.halving
entry unionfind_sequential_work_efficient_reverse_halving_bench = bench_unionfind_sequential_work_efficient.reverse_halving
entry unionfind_sequential_work_efficient_chunked_bench = bench_unionfind_sequential_work_efficient.chunked chunk_size

-- ==
-- entry: unionfind_sequential_work_efficient_find
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_all_random 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_all_linear 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_all_single 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_all_inverse_single 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_halving_random 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_halving_linear 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_halving_single 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_halving_inverse_single 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_reverse_halving_random 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_reverse_halving_linear 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_reverse_halving_single 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_reverse_halving_inverse_single 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_chunked_random 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_chunked_linear 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_chunked_single 100000i64 100000i64 }
-- no_cuda no_opencl no_hip no_multicore compiled script input { unionfind_sequential_work_efficient_chunked_inverse_single 100000i64 100000i64 }
entry unionfind_sequential_work_efficient_find = bench_unionfind_sequential_work_efficient.find 10
