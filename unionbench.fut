import "lib/github.com/diku-dk/containers/unionfind"

module type bench = {
  type handle
  type unionfind [n]
  val random : (n: i64) -> (m: i64) -> (*unionfind [n], [m](handle, handle))
  val linear : (n: i64) -> (*unionfind [n], [n](handle, handle))
}

module mk_bench (U: unionfind) : bench = {
  type handle = U.handle
  type unionfind [n] = U.unionfind [n]

  -- | Multiply-shift hash function https://arxiv.org/abs/1504.06804
  def hash (a: (u64, u64)) (b: (u64, u64)) (x: u64) : u64 =
    let y_mul_lo = a.0 * x
    in u64.mul_hi a.1 x + b.1 + u64.bool (y_mul_lo < b.0)

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

  def linear (n: i64) : (*unionfind [n], [n](handle, handle)) =
    let (uf, hs) = U.create n
    in (uf, zip hs (rotate 1 hs))

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
        let b = i64.max 0 (length hs - a)
        let (xs, ys) = sized (a + b) hs |> split
        let uf = U.union uf xs
        in (uf, ys, 2 * a)
    in uf
}
