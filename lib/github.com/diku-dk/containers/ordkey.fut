-- | Ordered key type.

module type ordkey = {
  -- | Context type.
  type~ ctx

  -- | Key type.
  type key

  -- | Less-than-or-equal.
  val lte : ctx -> key -> ctx -> key -> bool

  -- | Equality. This can be defined in terms of `lte`, but sometimes a more
  -- efficient implementation is possible.
  val eq : ctx -> key -> ctx -> key -> bool
}

-- | Comparable 64-bit integers.
module i64_key : ordkey with ctx = () with key = i64 = {
  type key = i64
  type ctx = ()

  def eq _ x _ y = x i64.== y
  def lte _ x _ y = x i64.<= y
}
