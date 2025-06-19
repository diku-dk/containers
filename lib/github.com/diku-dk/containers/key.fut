-- | Definitions of modules that implement both the `hashkey`@mtype@"hashkey"
-- and `ordkey`@mtype@"ordkey" module types for a variety of types.

import "../cpprandom/random"
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
  : key with ctx = () with key = P.t with hash = u64 = {
  type key = P.t
  type ctx = ()
  type hash = u64

  module params = {
    type t = u192
    def n : i64 = 2

    def mat =
      [ [ u192.from_u64 ([0u64, 4550289u64, 3114443612905851817u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 31521294u64, 4677272142619895914u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 4309077u64, 17725230307779815399u64] :> [u192.n]u64)
        ]
      , [ u192.from_u64 ([0u64, 32864941u64, 14067805869423985411u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 7570401u64, 4379138438894234111u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 31318884u64, 8032588423207853219u64] :> [u192.n]u64)
        ]
      ]
      :> [n][n + 1]u192
  }

  module engine = mk_ndimlcg u192 params

  type rng = engine.rng
  type const = [params.n]u192

  def rng_from_seed = engine.rng_from_seed

  def rand = engine.rand

  def hash _ (cs: const) (x: key) : hash =
    let x' = u64.i64 (P.to_i64 x)
    in universal.hash cs[0] cs[1] x'

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key : key with ctx = () with key = u8 with hash = u64 = mk_int_key u8
module u16key : key with ctx = () with key = u16 with hash = u64 = mk_int_key u16
module u32key : key with ctx = () with key = u32 with hash = u64 = mk_int_key u32
module u64key : key with ctx = () with key = u64 with hash = u64 = mk_int_key u64

module i8key : key with ctx = () with key = i8 with hash = u64 = mk_int_key i8
module i16key : key with ctx = () with key = i16 with hash = u64 = mk_int_key i16
module i32key : key with ctx = () with key = i32 with hash = u64 = mk_int_key i32
module i64key : key with ctx = () with key = i64 with hash = u64 = mk_int_key i64

local
module mk_int_key_u32
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with hash = u32 = {
  type key = P.t
  type ctx = ()
  type hash = u32

  module params = {
    type t = u128
    def n : i64 = 2

    def mat =
      [ [ u128.from_u64 ([4550289u64, 3114443612905851817u64] :> [u128.n]u64)
        , u128.from_u64 ([31521294u64, 4677272142619895914u64] :> [u128.n]u64)
        , u128.from_u64 ([4309077u64, 17725230307779815399u64] :> [u128.n]u64)
        ]
      , [ u128.from_u64 ([32864941u64, 14067805869423985411u64] :> [u128.n]u64)
        , u128.from_u64 ([7570401u64, 4379138438894234111u64] :> [u128.n]u64)
        , u128.from_u64 ([31318884u64, 8032588423207853219u64] :> [u128.n]u64)
        ]
      ]
      :> [n][n + 1]u128
  }

  module engine = mk_ndimlcg u128 params

  type rng = engine.rng
  type const = [params.n]u192

  def rng_from_seed = engine.rng_from_seed

  def rand = engine.rand

  #[inline]
  def hash _ (cs: const) (x: key) : hash =
    let x' = u32.i64 (P.to_i64 x)
    in universal_u32.hash cs[0] cs[1] x'

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

local
module u64key_u32
  (P: {
    type t

    val to_i64 : t -> i64
    val num_bits : i32
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with hash = u32 with const = (u128, u128) = {
  type key = P.t
  type ctx = ()
  type hash = u32
  type const = (u128, u128)

  def c : i64 = 1

  #[inline]
  def hash _ (a: [c]const) (x: key) : hash =
    let x' = u32.i64 (P.to_i64 x)
    in universal_hashing_u32.universal_hash a[0].0 a[0].1 x'

  def (==) (_, x) (_, y) = x P.== y
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key_u32 : key with ctx = () with key = u8 with hash = u32 with const = (u128, u128) = mk_int_key_u32 u8
module u16key_u32 : key with ctx = () with key = u16 with hash = u32 with const = (u128, u128) = mk_int_key_u32 u16
module u32key_u32 : key with ctx = () with key = u32 with hash = u32 with const = (u128, u128) = mk_int_key_u32 u32
module u64key_u32 : key with ctx = () with key = u64 with hash = u32 with const = (u128, u128) = mk_int_key_u32 u64

-- This is the error, need a smarter way of handling this case.

module i8key_u32 : key with ctx = () with key = i8 with hash = u32 with const = (u128, u128) = mk_int_key_u32 i8
module i16key_u32 : key with ctx = () with key = i16 with hash = u32 with const = (u128, u128) = mk_int_key_u32 i16
module i32key_u32 : key with ctx = () with key = i32 with hash = u32 with const = (u128, u128) = mk_int_key_u32 i32
module i64key_u32 : key with ctx = () with key = i64 with hash = u32 with const = (u128, u128) = mk_int_key_u32 i64
