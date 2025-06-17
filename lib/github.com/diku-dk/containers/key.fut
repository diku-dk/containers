-- | Definitions of modules that implement both the `hashkey`@mtype@"hashkey"
-- and `ordkey`@mtype@"ordkey" module types for a variety of types.

open import "hashkey"
open import "ordkey"
import "hash"

module type key = {
  type ctx
  type key
  type const
  type hash

  include hashkey
  with ctx = ctx
  with key = key
  with const = const
  with hash = hash

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
  : key with ctx = () with key = P.t with hash = u64 with const = u192 = {
  type key = P.t
  type ctx = ()
  type const = u192
  type hash = u64

  def c : i64 = 6

  -- https://lemire.me/blog/2018/08/15/fast-strongly-universal-64-bit-hashing-everywhere/
  def hash _ (a: [c]const) (x: key) : hash =
    let x' = u64.i64 (P.to_i64 x)
    in universal_hashing.universal_hash a[0] a[1] x'

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
