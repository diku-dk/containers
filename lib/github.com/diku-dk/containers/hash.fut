import "../cpprandom/random"

module type uint = {
  -- | Unsigned integral type.
  type t

  -- | Unsigned integral type that is smaller that t.
  type u

  -- | The number zero.
  val zero : t

  -- | A mersenne prime.
  val prime : t

  -- | Bitwise and.
  val (&) : t -> t -> t

  -- | Addition.
  val (+) : t -> t -> t

  -- | Multiplication.
  val (*) : t -> t -> t

  -- | Multiplication between t and u.
  val mul_small : t -> u -> t

  -- | Subtraction.
  val (-) : t -> t -> t

  -- | Greater than or equal.
  val (>=) : t -> t -> bool

  -- | Bitwise right shift by mersenne prime
  val shift_prime : t -> t

  -- | Converts u to t.
  val from_u : u -> t

  -- | Converts u to t.
  val to_u : t -> u

  -- | Number of u64 that encodes a t.
  val n : i64

  -- | Converts an array of u64 to t.
  val from_u64 : [n]u64 -> t

  -- | Converts t to an array of u64.
  val to_u64 : t -> [n]u64
}

module u128 : uint = {
  type t = {high: u64, low: u64}
  type u = u32

  #[inline]
  def zero =
    {high = 0u64, low = 0u64}

  -- | 2 ** 61 - 1
  #[inline]
  def prime =
    {high = 0u64, low = 0x1FFFFFFFFFFFFFFFu64}

  #[inline]
  def low (a: t) =
    a.low

  #[inline]
  def high (a: t) =
    a.high

  #[inline]
  def (&) (a: t) (b: t) =
    {high = high a & high b, low = low a & low b}

  -- | Shift by 61.
  #[inline]
  def shift_prime (a: t) =
    { high = high a >> 61
    , low = (low a u64.>> 61) u64.| (high a u64.<< 3)
    }

  #[inline]
  def (+) (a: t) (b: t) =
    let lo = (low a) + (low b)
    in { high = (high a) + (high b) + u64.bool (lo < low a)
       , low = lo
       }

  #[inline]
  def (-) (a: t) (b: t) =
    let lo = (low a) - (low b)
    in { high = (high a) - (high b) - u64.bool (lo > low a)
       , low = lo
       }

  #[inline]
  def (>=) (a: t) (b: t) : bool =
    (high a) > (high b) || ((high a) == (high b) && (low a) >= (low b))

  #[inline]
  def (>) (a: t) (b: t) : bool =
    (high a) > (high b) || ((high a) == (high b) && (low a) > (low b))

  def n : i64 = 2

  #[inline]
  def from_u64 (as: [n]u64) : t =
    {high = as[0], low = as[1]}

  #[inline]
  def from_u (a: u) : t =
    {high = 0, low = u64.u32 a}

  #[inline]
  def mul_small (a: t) (b: u) : t =
    let lo = u64.((low a) * (u32 b))
    let hi = u64.(mul_hi (low a) (u32 b) + (high a) * (u32 b))
    in {high = hi, low = lo}

  #[inline]
  def (*) (a: t) (b: t) : t =
    let lo = (low a) * (low b)
    let hi = u64.(mul_hi (low a) (low b) + (high a) * (low b) + (low a) * (high b))
    in {high = hi, low = lo}

  #[inline]
  def to_u (n: t) : u =
    let y =
      loop x = n
      while n.high != 0 do
        let lo = x.high u64.+ x.low
        in x with high = u64.bool (lo < x.high)
             with low = lo
    in u32.u64 y.low

  #[inline]
  def to_u64 (a: t) : [n]u64 =
    sized n [a.high, a.low]
}

type u128 = u128.t

module u32engine : rng_engine with t = u128 = {
  type t = u128
  module engine = xorshift128plus
  type rng = engine.rng

  def rand (x: rng) : (rng, u128) =
    let (x', a) = engine.rand x
    let (x'', b) = engine.rand x'
    in (x'', u128.(prime & from_u64 (sized n [a, b])))

  def rng_from_seed [n] (seed: [n]i32) =
    engine.rng_from_seed seed

  def split_rng (n: i64) (x: rng) : [n]rng =
    engine.split_rng n x

  def join_rng [n] (xs: [n]rng) : rng =
    engine.join_rng xs

  def min = u128.zero
  def max = u128.prime
}

module u192 : uint = {
  type t = {high: u64, mid: u64, low: u64}
  type u = u64

  #[inline]
  def zero =
    {high = 0u64, mid = 0u64, low = 0u64}

  -- | 2 ** 89 - 1
  #[inline]
  def prime =
    { high = 0u64
    , mid = 0x1FFFFFFu64
    , low = 0xFFFFFFFFFFFFFFFFu64
    }

  #[inline]
  def low (a: t) =
    a.low

  #[inline]
  def mid (a: t) =
    a.mid

  #[inline]
  def high (a: t) =
    a.high

  #[inline]
  def (&) (a: t) (b: t) : t =
    { high = high a & high b
    , mid = mid a & mid b
    , low = low a & low b
    }

  -- | Shift by 89
  #[inline]
  def shift_prime (a: t) =
    { high = 0u64
    , mid = high a >> 25
    , low = (low a u64.>> 25) u64.| (high a u64.<< 39)
    }

  #[inline]
  def (>=) (a: t) (b: t) : bool =
    (high a) > (high b)
    || ((high a) == (high b) && (mid a) > (mid b))
    || ((mid a) == (mid b) && (low a) >= (low b))

  def n : i64 = 3

  #[inline]
  def from_u64 (as: [n]u64) : t =
    {high = as[0], mid = as[1], low = as[2]}

  #[inline]
  def from_u (a: u) : t =
    {high = 0u64, mid = 0u64, low = a}

  #[inline]
  def mul_small (a: t) (b: u) : t =
    let lo = (low a) * b
    let mi' = (mid a) * b
    let mi = mi' + u64.mul_hi (low a) b
    let carry = u64.bool (mi < mi')
    let hi = (high a) * b + u64.mul_hi (mid a) b + carry
    in {high = hi, mid = mi, low = lo}

  #[inline]
  def (*) (a: t) (b: t) : t =
    let lo = (low a) * (low b)
    let mi'' = (mid a) * (low b)
    let mi' = mi'' + (mid b) * (low a)
    let mi = mi' + u64.mul_hi (low a) (low b)
    let carry = u64.bool (mi' < mi'') + u64.bool (mi < mi')
    let hi =
      (high a) * (low b)
      + (high b) * (low a)
      + (mid a) * (mid b)
      + u64.mul_hi (mid a) (low b)
      + u64.mul_hi (mid b) (low a)
      + carry
    in {high = hi, mid = mi, low = lo}

  #[inline]
  def (+) (a: t) (b: t) =
    let lo = (low a) + (low b)
    let mi = (mid a) + (mid b) + u64.bool (lo < low a)
    let hi = (high a) + (high b) + u64.bool (mi < mid a)
    in { high = hi
       , mid = mi
       , low = lo
       }

  #[inline]
  def (-) (a: t) (b: t) =
    let lo = (low a) - (low b)
    let mi = (mid a) - (mid b) - u64.bool (lo > low a)
    let hi = (high a) - (high b) - u64.bool (mi > mid a)
    in { high = hi
       , mid = mi
       , low = lo
       }

  #[inline]
  def to_u (n: t) : u =
    let y =
      loop x = n
      while n.high != 0 && n.mid != 0 do
        let lo = x.mid u64.+ x.low
        in x with high = 0u64
             with mid = x.high u64.+ u64.bool (lo < x.mid)
             with low = lo
    in y.low

  #[inline]
  def to_u64 (a: t) : [n]u64 =
    sized n [a.high, a.mid, a.low]
}

type u192 = u192.t

module u64engine : rng_engine with t = u192 = {
  type t = u192
  module engine = xorshift128plus
  type rng = engine.rng

  def rand (x: rng) : (rng, u192) =
    let (x', a) = engine.rand x
    let (x'', b) = engine.rand x'
    let (x''', c) = engine.rand x''
    in (x''', u192.(prime & from_u64 (sized n [a, b, c])))

  def rng_from_seed [n] (seed: [n]i32) =
    engine.rng_from_seed seed

  def split_rng (n: i64) (x: rng) : [n]rng =
    engine.split_rng n x

  def join_rng [n] (xs: [n]rng) : rng =
    engine.join_rng xs

  def min = u192.zero
  def max = u192.prime
}

module mk_universal_hashing (I: uint) = {
  type t = I.t
  type u = I.u

  -- | https://arxiv.org/pdf/2008.08654
  #[inline]
  def universal_hash (a: t) (b: t) (x: u) : u =
    -- Let b = 61 and u = 32, here 2**u - 1 is the largest u32 number
    -- and 2**b - 1 is the largest a value and the prime number.
    I.(let p = prime
       -- y < 2p(u - 1) + (p - 1) < (2u - 1)p <= p^2

       let y = a * from_u x + b
       -- y < p + p^2/2^b < 2p
       let y = (y & p) + shift_prime y
       -- y < p
       let y = if y >= p then y - p else y
       in to_u y)

  -- | test https://arxiv.org/pdf/1504.06804
  #[inline]
  def universal_hash_string [n] (a: t) (b: t) (c: t) (xs: [n]u) : u =
    -- Let b = 61 and u = 32, here 2**u - 1 is the largest u32 number
    -- and 2**b - 1 is the largest a value and the prime number.
    I.(let p = prime
       let y =
         -- Invariant y < 2p
         loop y = zero
         for x in xs do
           -- y < p**2 + 2**u - 1
           let y = y * c + (from_u x)
           -- y < p + (p**2 + 2**u - 1) / 2**b = p + p^2 / 2**b < 2p
           in (y & p) + shift_prime y
       -- y < p
       let y = if y >= p then y - p else y
       -- Use universal_hash
       -- y < p**2 + p
       let y = a * y + b
       -- y < p + (p**2 + p) / 2**b < 2p
       let y = (y & p) + shift_prime y
       -- y < p
       let y = if y >= p then y - p else y
       in to_u y)
}

module universal_hashing_u32 = mk_universal_hashing u128
module universal_hashing = mk_universal_hashing u192
