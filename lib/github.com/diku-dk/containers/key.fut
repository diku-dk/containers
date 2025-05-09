-- | Key module.
--
-- | This module specifies what key needs to be defined for it to be
-- used in datastructures which use hash functions.

module type key = {
  -- | Constants type.
  type i

  -- | Key type.
  type k

  -- | Number constants the hash function is depended on.
  val m: i64

  -- | Equality definition for the key.
  val eq : k -> k -> bool

  -- | A given hash function use.
  val hash : [m]i -> k -> i64
}
