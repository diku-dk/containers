-- | Types representing slices of arrays.
--
-- This module defines various module types and modules for working with slices
-- of arrays at different levels of granularity. None of the functionality here
-- is particularly advanced or difficult to implement, but it is useful as a
-- common interface for other containers.

import "array"
import "key"

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
  type uint
  val encode : t -> uint
}

module mk_encoder_params
  (U: integral)
  (I: integral)
  : encoder with t = I.t with uint = U.t = {
  type t = I.t
  type uint = U.t
  def encode = U.i64 <-< I.to_i64
}

module mk_encoder = mk_encoder_params u64
module mk_encoder_u32 = mk_encoder_params u32

-- | Create a `key`@mtype@"key" for slices of some specific element type. The
-- element type must also be provided with a `key`@mtype module for which the
-- context is unit.
module mk_slice_key
  (K: ordkey with ctx = ())
  (E: encoder with t = K.key with uint = u64)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with uint = u64 = {
  type key = slice.slice K.key
  type~ ctx = ?[l].[l]K.key
  type uint = u64

  def c : i64 = 33

  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  def hash (ctx: []K.key) (a: [c]uint) (x: key) : uint =
    let data = map E.encode (slice.get x ctx)
    in loop h = a[0]
       for (x', i) in zip data (indices data) do
         h + x' * a[1 + i % 32]
}

module mk_slice_key_u32
  (K: ordkey with ctx = ())
  (E: encoder with t = K.key with uint = u32)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with uint = u32 = {
  type key = slice.slice K.key
  type~ ctx = ?[l].[l]K.key
  type uint = u32

  def c : i64 = 33

  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  def hash (ctx: []K.key) (a: [c]uint) (x: key) : uint =
    let data = map E.encode (slice.get x ctx)
    in loop h = a[0]
       for (x', i) in zip data (indices data) do
         h + x' * a[1 + i % 32]
}
