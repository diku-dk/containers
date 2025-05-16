-- | Key module.
--
-- | This module specifies what key needs to be defined for it to be
-- used in datastructures which use hash functions.

module type key = {
  -- | Context type.
  type~ ctx

  -- | Key type.
  type k

  -- | Number constants the hash function is depended on.
  val m : i64

  -- | Equality definition for the key.
  val eq : ctx -> k -> k -> bool

  -- | A given hash function use.
  val hash : ctx -> [m]u64 -> k -> u64
}
