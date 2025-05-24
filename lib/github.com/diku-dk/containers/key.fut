-- | Definitions of modules that implement both the `hashkey`@mtype@"hashkey"
-- and `ordkey`@mtype@"ordkey" module types for a variety of types.

open import "hashkey"
open import "ordkey"

module type key = {
  type ctx
  type key

  include hashkey
  with ctx = ctx
  with key = key

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
  : key with ctx = () with key = P.t = {
  type key = P.t
  type ctx = ()

  def m : i64 = 1

  -- The hash function was found [here](http://stackoverflow.com/a/12996028).
  def hash _ (a: [m]u64) (x: key) : u64 =
    let x = a[0] * u64.i64 (P.to_i64 x)
    let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
    let x = (x ^ (x >> 27)) * 0x94d049bb133111eb
    let y = (x ^ (x >> 31))
    in y

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key : key with ctx = () with key = u8 = mk_int_key u8
module u16key : key with ctx = () with key = u16 = mk_int_key u16
module u32key : key with ctx = () with key = u32 = mk_int_key u32
module u64key : key with ctx = () with key = u64 = mk_int_key u64

module i8key : key with ctx = () with key = i8 = mk_int_key i8
module i16key : key with ctx = () with key = i16 = mk_int_key i16
module i32key : key with ctx = () with key = i32 = mk_int_key i32
module i64key : key with ctx = () with key = i64 = mk_int_key i64
