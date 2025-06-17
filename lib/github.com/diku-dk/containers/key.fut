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

  def c : i64 = 2

  def hash _ (a: [c]const) (x: key) : hash =
    let x' = u64.i64 (P.to_i64 x)
    in universal_hashing.universal_hash a[0] a[1] x'

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key : key with ctx = () with key = u8 with hash = u64 with const = u192 = mk_int_key u8
module u16key : key with ctx = () with key = u16 with hash = u64 with const = u192 = mk_int_key u16
module u32key : key with ctx = () with key = u32 with hash = u64 with const = u192 = mk_int_key u32
module u64key : key with ctx = () with key = u64 with hash = u64 with const = u192 = mk_int_key u64

module i8key : key with ctx = () with key = i8 with hash = u64 with const = u192 = mk_int_key i8
module i16key : key with ctx = () with key = i16 with hash = u64 with const = u192 = mk_int_key i16
module i32key : key with ctx = () with key = i32 with hash = u64 with const = u192 = mk_int_key i32
module i64key : key with ctx = () with key = i64 with hash = u64 with const = u192 = mk_int_key i64

local
module mk_int_key_u32
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with hash = u32 with const = u128 = {
  type key = P.t
  type ctx = ()
  type hash = u32
  type const = u128

  def c : i64 = 2

  #[inline]
  def hash _ (a: [c]const) (x: key) : hash =
    let x' = u32.i64 (P.to_i64 x)
    in universal_hashing_u32.universal_hash a[0] a[1] x'

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key_u32 : key with ctx = () with key = u8 with hash = u32 with const = u128 = mk_int_key_u32 u8
module u16key_u32 : key with ctx = () with key = u16 with hash = u32 with const = u128 = mk_int_key_u32 u16
module u32key_u32 : key with ctx = () with key = u32 with hash = u32 with const = u128 = mk_int_key_u32 u32
module u64key_u32 : key with ctx = () with key = u64 with hash = u32 with const = u128 = mk_int_key_u32 u64

module i8key_u32 : key with ctx = () with key = i8 with hash = u32 with const = u128 = mk_int_key_u32 i8
module i16key_u32 : key with ctx = () with key = i16 with hash = u32 with const = u128 = mk_int_key_u32 i16
module i32key_u32 : key with ctx = () with key = i32 with hash = u32 with const = u128 = mk_int_key_u32 i32
module i64key_u32 : key with ctx = () with key = i64 with hash = u32 with const = u128 = mk_int_key_u32 i64
