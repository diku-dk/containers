import "lib/github.com/diku-dk/containers/unionfind"

module type bench = {
  type handle
  type unionfind [n]
  val random : (n: i64) -> (m: i64) -> (*unionfind [n], [m](handle, handle))
  val linear : (n: i64) -> (m: i64) -> (*unionfind [n], [m](handle, handle))
  val single : (n: i64) -> (m: i64) -> (*unionfind [n], [m](handle, handle))
  val inverse_single : (n: i64) -> (m: i64) -> (*unionfind [n], [m](handle, handle))
  val all [n] [m] : *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val halving [n] [m] : *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val reverse_halving [n] [m] : *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val chunked [n] [m] : i64 -> *unionfind [n] -> [m](handle, handle) -> *unionfind [n]
  val find [n] [m] : i64 -> *unionfind [n] -> [m]handle -> *unionfind [n]
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

  def random (n: i64) (m: i64) : (*unionfind [n], [m](handle, handle)) =
    let (uf, hs) = U.create n
    let eqs =
      equations n m
      |> map (\(i, j) -> (hs[i], hs[j]))
    in (uf, eqs)

  def single (n: i64) (m: i64) : (*unionfind [n], [m](handle, handle)) =
    let (uf, hs) = U.create n
    let hs' = take m hs
    in (uf, map (\h -> (hs'[0], h)) hs')

  def inverse_single (n: i64) (m: i64) : (*unionfind [n], [m](handle, handle)) =
    let (uf, hs) = U.create n
    let hs' = take m hs
    in (uf, map (\h -> (h, hs'[0])) hs')

  def linear (n: i64) (m: i64) : (*unionfind [n], [m](handle, handle)) =
    let (uf, hs) = U.create n
    let hs' = take m hs
    in (uf, zip hs' (rotate 1 hs'))

  def all [n] [m] (uf: *unionfind [n]) (hs: [m](handle, handle)) =
    U.union uf hs

  def halving [n] [m] (uf: *unionfind [n]) (hs: [m](handle, handle)) =
    let (uf, _) =
      loop (uf, hs) while length hs != 0 do
        let a = 1 + (length hs - 1) / 2
        let b = length hs / 2
        let (xs, ys) = sized (a + b) hs |> split
        let uf = U.union uf xs
        in (uf, ys)
    in uf

  def reverse_halving [n] [m] (uf: *unionfind [n]) (hs: [m](handle, handle)) =
    let (uf, _, _) =
      loop (uf, hs, a) = (uf, hs, 1)
      while length hs != 0 do
        let k = length hs
        let b = i64.max 0 (k - a)
        let (xs, ys) = sized (i64.min k a + b) hs |> split
        let uf = U.union uf xs
        in (uf, ys, 2 * a)
    in uf

  def chunked [n] [m] (step: i64) (uf: *unionfind [n]) (hs: [m](handle, handle)) =
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
    loop uf for _i < steps do
      let (uf, _) = U.find uf hs
      in uf

  def compose f g num_vars num_eqs =
    let (uf, eqs) = f num_vars num_eqs
    let uf' = g uf eqs
    in (uf', U.handles uf')
}

module bench_unionfind = mk_bench unionfind

entry unionfind_random = bench_unionfind.random
entry unionfind_linear = bench_unionfind.linear
entry unionfind_single = bench_unionfind.single
entry unionfind_inverse_single = bench_unionfind.inverse_single

-- ==
-- entry: unionfind_all_bench unionfind_halving_bench unionfind_reverse_halving_bench unionfind_chunked_1000_bench
-- script input { unionfind_random 1000000i64 200000i64 }
-- script input { unionfind_linear 1000000i64 200000i64 }
-- script input { unionfind_single 1000000i64 200000i64 }
-- script input { unionfind_inverse_single 1000000i64 200000i64 }
entry unionfind_all_bench = bench_unionfind.all
entry unionfind_halving_bench = bench_unionfind.halving
entry unionfind_reverse_halving_bench = bench_unionfind.reverse_halving
entry unionfind_chunked_1000_bench = bench_unionfind.chunked 1000i64

module bench_unionfind_by_size = mk_bench unionfind_by_size

entry unionfind_by_size_random = bench_unionfind_by_size.random
entry unionfind_by_size_linear = bench_unionfind_by_size.linear
entry unionfind_by_size_single = bench_unionfind_by_size.single
entry unionfind_by_size_inverse_single = bench_unionfind_by_size.inverse_single

-- ==
-- entry: unionfind_by_size_all_bench unionfind_by_size_halving_bench unionfind_by_size_reverse_halving_bench unionfind_by_size_chunked_1000_bench
-- script input { unionfind_by_size_random 1000000i64 200000i64 }
-- script input { unionfind_by_size_linear 1000000i64 200000i64 }
-- script input { unionfind_by_size_single 1000000i64 200000i64 }
-- script input { unionfind_by_size_inverse_single 1000000i64 200000i64 }
entry unionfind_by_size_all_bench = bench_unionfind_by_size.all
entry unionfind_by_size_halving_bench = bench_unionfind_by_size.halving
entry unionfind_by_size_reverse_halving_bench = bench_unionfind_by_size.reverse_halving
entry unionfind_by_size_chunked_1000_bench = bench_unionfind_by_size.chunked 1000i64

module bench_unionfind_by_rank = mk_bench unionfind_by_rank

entry unionfind_by_rank_random = bench_unionfind_by_rank.random
entry unionfind_by_rank_linear = bench_unionfind_by_rank.linear
entry unionfind_by_rank_single = bench_unionfind_by_rank.single
entry unionfind_by_rank_inverse_single = bench_unionfind_by_rank.inverse_single

-- ==
-- entry: unionfind_by_rank_all_bench unionfind_by_rank_halving_bench unionfind_by_rank_reverse_halving_bench unionfind_by_rank_chunked_1000_bench
-- script input { unionfind_by_rank_random 1000000i64 200000i64 }
-- script input { unionfind_by_rank_linear 1000000i64 200000i64 }
-- script input { unionfind_by_rank_single 1000000i64 200000i64 }
-- script input { unionfind_by_rank_inverse_single 1000000i64 200000i64 }
entry unionfind_by_rank_all_bench = bench_unionfind_by_rank.all
entry unionfind_by_rank_halving_bench = bench_unionfind_by_rank.halving
entry unionfind_by_rank_reverse_halving_bench = bench_unionfind_by_rank.reverse_halving
entry unionfind_by_rank_chunked_1000_bench = bench_unionfind_by_rank.chunked 1000i64

module bench_unionfind_by_rank_alternative = mk_bench unionfind_by_rank_alternative

entry unionfind_by_rank_alternative_random = bench_unionfind_by_rank_alternative.random
entry unionfind_by_rank_alternative_linear = bench_unionfind_by_rank_alternative.linear
entry unionfind_by_rank_alternative_single = bench_unionfind_by_rank_alternative.single
entry unionfind_by_rank_alternative_inverse_single = bench_unionfind_by_rank_alternative.inverse_single

-- ==
-- entry: unionfind_by_rank_alternative_all_bench unionfind_by_rank_alternative_halving_bench unionfind_by_rank_alternative_reverse_halving_bench unionfind_by_rank_alternative_chunked_1000_bench
-- script input { unionfind_by_rank_alternative_random 1000000i64 200000i64 }
-- script input { unionfind_by_rank_alternative_linear 1000000i64 200000i64 }
-- script input { unionfind_by_rank_alternative_single 1000000i64 200000i64 }
-- script input { unionfind_by_rank_alternative_inverse_single 1000000i64 200000i64 }
entry unionfind_by_rank_alternative_all_bench = bench_unionfind_by_rank_alternative.all
entry unionfind_by_rank_alternative_halving_bench = bench_unionfind_by_rank_alternative.halving
entry unionfind_by_rank_alternative_reverse_halving_bench = bench_unionfind_by_rank_alternative.reverse_halving
entry unionfind_by_rank_alternative_chunked_1000_bench = bench_unionfind_by_rank_alternative.chunked 1000i64

module bench_unionfind_sequential_work_efficient = mk_bench unionfind_sequential_work_efficient

entry unionfind_sequential_work_efficient_random = bench_unionfind_sequential_work_efficient.random
entry unionfind_sequential_work_efficient_linear = bench_unionfind_sequential_work_efficient.linear
entry unionfind_sequential_work_efficient_single = bench_unionfind_sequential_work_efficient.single
entry unionfind_sequential_work_efficient_inverse_single = bench_unionfind_sequential_work_efficient.inverse_single

-- ==
-- entry: unionfind_sequential_work_efficient_all_bench unionfind_sequential_work_efficient_halving_bench unionfind_sequential_work_efficient_reverse_halving_bench unionfind_sequential_work_efficient_chunked_1000_bench
-- no_cuda no_opencl no_hip script input { unionfind_sequential_work_efficient_random 100000i64 20000i64 }
-- no_cuda no_opencl no_hip script input { unionfind_sequential_work_efficient_linear 100000i64 20000i64 }
-- no_cuda no_opencl no_hip script input { unionfind_sequential_work_efficient_single 100000i64 20000i64 }
-- no_cuda no_opencl no_hip script input { unionfind_sequential_work_efficient_inverse_single 100000i64 20000i64 }
entry unionfind_sequential_work_efficient_all_bench = bench_unionfind_sequential_work_efficient.all
entry unionfind_sequential_work_efficient_halving_bench = bench_unionfind_sequential_work_efficient.halving
entry unionfind_sequential_work_efficient_reverse_halving_bench = bench_unionfind_sequential_work_efficient.reverse_halving
entry unionfind_sequential_work_efficient_chunked_1000_bench = bench_unionfind_sequential_work_efficient.chunked 1000i64
