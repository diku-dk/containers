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

-- | Create a `key`@mtype@"key" for slices of some specific element type. The
-- element type must also be provided with a `key`@mtype module for which the
-- context is unit.
module mk_slice_key (E: key with ctx = () with uint = u64)
  : key
    with ctx = []E.key
    with key = slice.slice E.key
    with uint = u64 = {
  type i = u64
  type key = slice.slice E.key
  type~ ctx = ?[l].[l]E.key
  type uint = u64

  def c : i64 = E.c

  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) E.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) E.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  def hash (ctx: []E.key) (a: [c]uint) (x: key) : uint =
    -- Fowler–Noll–Vo (FNV) style hash combining
    loop h = 2166136261
    for x' in slice.get x ctx do
      (h * 16777619) ^ E.hash () (sized E.c a) x'
}
