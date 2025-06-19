-- | Types representing slices of arrays.
--
-- This module defines various module types and modules for working with slices
-- of arrays at different levels of granularity. None of the functionality here
-- is particularly advanced or difficult to implement, but it is useful as a
-- common interface for other containers.

import "array"
import "key"
import "hash"

-- | An array slice represents a contiguous interval of some array. Essentially,
-- a slice is an offset and a length. The slice does not contain a reference to
-- the array in question. This is mostly useful for working with arrays of
-- slices, as this would otherwise lead to irregular arrays-of-arrays.
module type slice = {
  -- | An array slice. The `a` parameter is merely a phantom type.
  type slice 'a

  -- | Construct an array slice from an offset and a length.
  val mk 'a : (i: i64) -> (n: i64) -> slice a

  -- | Deconstruct array slice into offset and length. The following property
  -- holds:
  --
  -- ```
  -- unmk (mk i n) == (i,n)
  -- ```
  val unmk 'a : slice a -> (i64, i64)

  -- | Retrieve the elements corresponding to some slice of an array. This may
  -- fail if the slide is out-of-bounds.
  val get [n] 'a : slice a -> [n]a -> ?[k].[k]a
}

module slice : slice = {
  type slice 'a = {i: i64, n: i64}
  def mk i n = {i, n}
  def unmk {i, n} = (i, n)
  def get {i, n} xs = xs[i:i + n]
}

module type encoder = {
  type t
  type u
  val get : i64 -> t -> u
  val num : t -> i64
}

module mk_encoder_params
  (U: integral)
  (I: integral)
  : encoder with u = U.t with t = []I.t = {
  type~ t = ?[n].[n]I.t
  type u = U.t

  def to_u = U.i64 <-< I.to_i64

  def num_i_per_u : i64 = i64.i32 (U.num_bits / I.num_bits)

  def num_u_per_i : i64 = i64.i32 (I.num_bits / U.num_bits)

  def num (ts: t) : i64 =
    if U.num_bits != I.num_bits
    then (length ts + num_i_per_u - 1) / num_i_per_u
    else length ts * num_u_per_i

  def get (i: i64) (ts: t) : u =
    if num_i_per_u != 0
    then #[unroll]
         loop x = U.i64 0
         for j in 0..<num_i_per_u do
           let idx = i * num_i_per_u + j
           in if idx < length ts
              then let value = U.((to_u ts[idx]) << (i64 j * i32 I.num_bits))
                   in x U.| value
              else x
    else let idx = i / num_u_per_i
         let part = U.(i64 i %% i64 num_u_per_i)
         in if idx < length ts
            then let full_value = to_u ts[idx]
                 let shift_amount = U.(part * i32 num_bits)
                 let mask = U.(U.i64 1 << i32 num_bits - i64 1)
                 in U.(full_value >> shift_amount & mask)
            else U.i64 0
}

module mk_encoder = mk_encoder_params u64
module mk_encoder_u32 = mk_encoder_params u32

-- | Create a `key`@mtype@"key" for slices of some specific element type. The
-- element type must also be provided with a `key`@mtype module for which the
-- context is unit.
module mk_slice_key
  (K: ordkey with ctx = ())
  (E: encoder with t = []K.key with u = u64)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with hash = u64 = {
  type key = slice.slice K.key
  type hash = u64
  type~ ctx = ?[l].[l]K.key

  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  module params = {
    type t = u192
    def n : i64 = 3

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

  def hash (ctx: []K.key) (a: const) (x: key) : hash =
    let data = slice.get x ctx
    let n = E.num data
    in universal.hash_string a[0] a[1] a[2] E.get n data
}

module mk_slice_key_u32
  (K: ordkey with ctx = ())
  (E: encoder with t = []K.key with u = u32)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with hash = u32
    with const = u128 = {
  type key = slice.slice K.key
  type~ ctx = ?[l].[l]K.key
  type uint = u32
  type const = u128
  type hash = u32

  def c : i64 = 3

  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  def hash (ctx: []K.key) (a: [c]const) (x: key) : hash =
    let data = slice.get x ctx
    let n = E.num data
    in universal.hash_string a[0] a[1] a[2] E.get n data
}
