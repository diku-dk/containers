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

  def c : i64 = 4

  -- 2^61 - 1
  def prime : u64 = 0x1FFFFFFFFFFFFFFF

  def mod_prime a = a %% prime

  def concat a b =
    mod_prime ((u64.u32 a << 32) + u64.u32 b)

  def constants arr =
    ( concat arr[0] arr[1]
    , concat arr[2] arr[3]
    )

  def hash _ (a: [c]uint) (x: key) : uint =
    let (a', b') = constants a
    let y = (u64.max 1 a') * u64.i64 (P.to_i64 x) + b'
    in u32.u64 (mod_prime y)

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
