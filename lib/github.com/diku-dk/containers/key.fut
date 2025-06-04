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

  def m : i64 = 1

  -- The hash function was found [here](http://stackoverflow.com/a/12996028).
  def hash _ (a: [m]uint) (x: key) : uint =
    let x = (u64.max 1 a[0]) * u64.i64 (P.to_i64 x)
    let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
    let x = (x ^ (x >> 27)) * 0x94d049bb133111eb
    let y = (x ^ (x >> 31))
    in y

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

  def m : i64 = 1

  -- The hash function was found [here](http://stackoverflow.com/a/12996028).
  def hash _ (a: [m]uint) (x: key) : uint =
    let x = (u32.max 1 a[0]) * u32.i64 (P.to_i64 x)
    let x = (x ^ (x >> 16)) * 0x119de1f3
    let x = (x ^ (x >> 16)) * 0x119de1f3
    let y = (x ^ (x >> 16))
    in y

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
