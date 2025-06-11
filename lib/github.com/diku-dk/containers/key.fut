-- | Definitions of modules that implement both the `hashkey`@mtype@"hashkey"
-- and `ordkey`@mtype@"ordkey" module types for a variety of types.

open import "hashkey"
open import "ordkey"

module type key = {
  type ctx
  type key
  type uint

  include hashkey
  with ctx = ctx
  with key = key
  with uint = uint

  include ordkey
  with ctx = ctx
  with key = key
}

local
module mk_int_key
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with uint = u64 = {
  type key = P.t
  type ctx = ()
  type uint = u64

  def c : i64 = 6

  -- https://lemire.me/blog/2018/08/15/fast-strongly-universal-64-bit-hashing-everywhere/
  def hash _ (a: [c]uint) (x: key) : uint =
    let x' = u64.i64 (P.to_i64 x)
    let low = x'
    let high = x' >> 32
    let low_hash = (a[0] * low + a[1] * high + a[2]) >> 32
    let high_hash = (a[3] * low + a[4] * high + a[5]) & 0xFFFFFFFF00000000
    in low_hash | high_hash

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key : key with ctx = () with key = u8 with uint = u64 = mk_int_key u8
module u16key : key with ctx = () with key = u16 with uint = u64 = mk_int_key u16
module u32key : key with ctx = () with key = u32 with uint = u64 = mk_int_key u32
module u64key : key with ctx = () with key = u64 with uint = u64 = mk_int_key u64

module i8key : key with ctx = () with key = i8 with uint = u64 = mk_int_key i8
module i16key : key with ctx = () with key = i16 with uint = u64 = mk_int_key i16
module i32key : key with ctx = () with key = i32 with uint = u64 = mk_int_key i32
module i64key : key with ctx = () with key = i64 with uint = u64 = mk_int_key i64

local
module u128 = {
  type u128 = {high: u64, low: u64}

  #[inline]
  def (&) (a: u128) (b: u128) =
    {high = a.high & b.high, low = a.low & b.low}

  #[inline]
  def (|) (a: u128) (b: u128) =
    {high = a.high | b.high, low = a.low | b.low}

  #[inline]
  def (<<) (a: u128) (b: u128) =
    if b.high != 0 || b.low >= 128
    then {high = 0, low = 0}
    else if b.low >= 64
    then {high = a.low << (b.low - 64), low = 0}
    else { high = (a.high u64.<< b.low) u64.| (a.low >> (64 - b.low))
         , low = a.low << b.low
         }

  #[inline]
  def (>>) (a: u128) (b: u128) =
    if b.high != 0 || b.low >= 128
    then {high = 0, low = 0}
    else if b.low >= 64
    then {high = 0, low = a.high >> (b.low - 64)}
    else { high = a.high >> b.low
         , low = (a.low u64.>> b.low) u64.| (a.high u64.<< (64 - b.low))
         }

  #[inline]
  def (+) (a: u128) (b: u128) =
    let low = a.low + b.low
    in { high = a.high + b.high + u64.bool (low < a.low)
       , low = low
       }

  #[inline]
  def (-) (a: u128) (b: u128) =
    let low = a.low - b.low
    in { high = a.high - b.high - u64.bool (low > a.low)
       , low = low
       }

  #[inline]
  def (<=) (a: u128) (b: u128) : bool =
    a.high < b.high || (a.high == b.high && a.low <= b.low)

  #[inline]
  def (>=) (a: u128) (b: u128) : bool =
    a.high > b.high || (a.high == b.high && a.low >= b.low)

  #[inline]
  def (>) (a: u128) (b: u128) : bool =
    a.high > b.high || (a.high == b.high && a.low > b.low)

  #[inline]
  def (<) (a: u128) (b: u128) : bool =
    a.high < b.high || (a.high == b.high && a.low < b.low)

  #[inline]
  def (==) (a: u128) (b: u128) : bool =
    a.high == b.high && a.low == b.low

  #[inline]
  def from_u64 (a: u64) : u128 =
    {high = 0, low = a}

  #[inline]
  def from_2_u64 (high: u64) (low: u64) : u128 =
    {high = high, low = low}

  #[inline]
  def from_4_u32 (high0: u32) (high1: u32) (low0: u32) (low1: u32) : u128 =
    u64.({ high = (u32 high0) << 32 | u32 high1
         , low = (u32 low0) << 32 | u32 low1
         })

  #[inline]
  def mod_2p_minus_1 (n: u128) (p: u128) : u128 =
    let modulus = (from_u64 1u64 << p) - from_u64 1
    let n_final =
      loop n while n > modulus do (n >> p) + (n & modulus)
    in if n_final == modulus
       then from_u64 0u64
       else n_final

  #[inline]
  def (*) (a: u128) (b: u128) : u128 =
    let low = a.low * b.low
    let high = u64.(mul_hi a.low b.low + a.high * b.low + a.low * b.high)
    in {high = high, low = low}

  #[inline]
  def to_u32 (n: u128) : u32 =
    let result = mod_2p_minus_1 n (from_u64 32)
    in u32.u64 result.low

  #[inline]
  def max (a: u128) (b: u128) : u128 =
    if a > b then a else b

  #[inline]
  def min (a: u128) (b: u128) : u128 =
    if a < b then a else b
}

local
module mk_int_key_u32
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with uint = u32 = {
  type key = P.t
  type ctx = ()
  type uint = u32

  def c : i64 = 8

  -- 2^61 - 1

  #[inline]
  def prime : u128.u128 = u128.from_u64 0x1FFFFFFFFFFFFFFF

  #[inline]
  def hash _ (a: [c]uint) (x: key) : uint =
    let a' = prime u128.& u128.from_4_u32 a[0] a[1] a[2] a[3]
    let b' = prime u128.& u128.from_4_u32 a[4] a[5] a[6] a[7]
    let x' = u128.from_u64 (u64.i64 (P.to_i64 x))
    let y = (u128.max (u128.from_u64 1u64) a') u128.* x' u128.+ b'
    in u128.to_u32 (u128.mod_2p_minus_1 y prime)

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key_u32 : key with ctx = () with key = u8 with uint = u32 = mk_int_key_u32 u8
module u16key_u32 : key with ctx = () with key = u16 with uint = u32 = mk_int_key_u32 u16
module u32key_u32 : key with ctx = () with key = u32 with uint = u32 = mk_int_key_u32 u32
module u64key_u32 : key with ctx = () with key = u64 with uint = u32 = mk_int_key_u32 u64

module i8key_u32 : key with ctx = () with key = i8 with uint = u32 = mk_int_key_u32 i8
module i16key_u32 : key with ctx = () with key = i16 with uint = u32 = mk_int_key_u32 i16
module i32key_u32 : key with ctx = () with key = i32 with uint = u32 = mk_int_key_u32 i32
module i64key_u32 : key with ctx = () with key = i64 with uint = u32 = mk_int_key_u32 i64
