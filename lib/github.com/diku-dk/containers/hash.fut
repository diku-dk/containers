module u128 = {
  type t = {high: u64, low: u64}
  type u = u32

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

  #[inline]
  def shift_prime (a: t) =
    { high = high a >> 61
    , low = (low a u64.>> 61) u64.| (high a u64.<< 3)
    }

  #[inline]
  def (+) (a: t) (b: t) =
    let lo = (low a) + (low b)
    in { high = (high a) + (high b) + u64.bool (lo < (low a))
       , low = lo
       }

  #[inline]
  def (-) (a: t) (b: t) =
    let lo = (low a) - (low b)
    in { high = (high a) - (high b) - u64.bool (lo > (low a))
       , low = lo
       }

  #[inline]
  def (>=) (a: t) (b: t) : bool =
    (high a) > (high b) || ((high a) == (high b) && (low a) >= (low b))

  #[inline]
  def from_u64 (a: u64) : t =
    {high = 0, low = a}

  #[inline]
  def from_u32 (a: u32) : t =
    {high = 0, low = u64.u32 a}

  #[inline]
  def from_two_u64 (hi: u64) (lo: u64) : t =
    {high = hi, low = lo}

  #[inline]
  def mul_small (a: t) (b: u) : t =
    let lo = (low a) * (u64.u32 b)
    let hi = u64.(mul_hi (low a) (u32 b) + (high a) * (u32 b))
    in {high = hi, low = lo}

  #[inline]
  def (*) (a: t) (b: t) : t =
    let lo = (low a) * (low b)
    let hi = u64.(mul_hi (low a) (low b) + (high a) * (low b) + (low a) * (high b))
    in {high = hi, low = lo}

  -- | Converts a u128 to u32
  -- precondition: n < 2**64 - 1
  #[inline]
  def to_u (n: t) : u32 =
    u32.u64 n.low
}

type u128 = u128.t

-- | https://arxiv.org/pdf/2008.08654
#[inline]
def universal_hash_u32 (a: u128) (b: u128) (x: u32) : u32 =
  -- Let b = 61 and u = 32, here 2**u - 1 is the largest u32 number
  -- and 2**b - 1 is the largest a value and the prime number.
  u128.(-- y < 2p(u - 1) + (p - 1) < (2u - 1)p <= p^2
        let p = prime
        let y = a * from_u32 x + b
        -- y < p + p^2/2^b < 2p
        let y = (y & p) + shift_prime y
        -- y < p
        let y = if y >= p then y - p else y
        in to_u y)

-- | test https://arxiv.org/pdf/1504.06804
#[inline]
def universal_hash_string_u32 [n] (a: u128) (b: u128) (c: u128) (xs: [n]u32) =
  -- Let b = 61 and u = 32, here 2**u - 1 is the largest u32 number
  -- and 2**b - 1 is the largest a value and the prime number.
  u128.(let p = prime
        let y =
          -- Invariant y < 2p
          loop y = from_u32 0
          for x in xs do
            -- y < p**2 + 2**u - 1
            let y = y * c + (from_u32 x)
            -- y < p + (p**2 + 2**u - 1) / 2**b = p + p^2 / 2**b < 2p
            in (y & p) + shift_prime y
        -- y < p
        let y = if y >= p then y - p else y
        -- Use universal_hash_u32
        -- y < p**2 + p
        let y = a * y + b
        -- y < p + (p**2 + p) / 2**b < 2p
        let y = (y & p) + shift_prime y
        -- y < p
        let y = if y >= p then y - p else y
        in to_u y)

module u192 = {
  type t = {high: u64, mid: u64, low: u64}
  type u = u64

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
  def (&) (a: t) (b: t) =
    {high = high a & high b, mid = mid a & mid b, low = low a & low b}

  #[inline]
  def shift_prime (a: t) =
    { high = high a >> 89
    , mid = (mid a u64.>> 25) u64.| (high a u128.<< (128 - b))
    , low = (low a u64.>> b) u64.| (high a u128.<< (64 - b))
    }

  #[inline]
  def (+) (a: t) (b: t) =
    let lo = (low a) + (low b)
    in { high = (high a) + (high b) + u64.bool (lo < (low a))
       , low = lo
       }

  #[inline]
  def (-) (a: t) (b: t) =
    let lo = (low a) - (low b)
    in { high = (high a) - (high b) - u64.bool (lo > (low a))
       , low = lo
       }

  #[inline]
  def (>=) (a: t) (b: t) : bool =
    (high a) > (high b) || ((high a) == (high b) && (low a) >= (low b))

  #[inline]
  def from_u64 (a: u64) : t =
    {high = 0, low = a}

  #[inline]
  def from_u32 (a: u32) : t =
    {high = 0, low = u64.u32 a}

  #[inline]
  def from_two_u64 (hi: u64) (lo: u64) : t =
    {high = hi, low = lo}

  #[inline]
  def (*) (a: t) (b: t) : t =
    let lo = (low a) * (low b)
    let hi = u64.(mul_hi (low a) (low b) + (high a) * (low b) + (low a) * (high b))
    in {high = hi, low = lo}

  -- | Converts a u128 to u32
  -- precondition: n < 2**64 - 1
  #[inline]
  def to_u32 (n: t) : u32 =
    u32.u64 n.low
}
