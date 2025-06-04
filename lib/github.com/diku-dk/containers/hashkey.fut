-- | Key module.
--
-- | This module specifies what key needs to be defined for it to be used in
-- data structures which use hash functions, such as
-- `mk_hashmap`@term@"hashmap"..

-- A module type for keys that can be hashed.
module type hashkey = {
  -- | Context type.
  type~ ctx

  -- | Key type.
  type key

  -- | The hash type
  type uint

  -- | Number constants the hash function is depended on.
  val m : i64

  -- | Equality definition for the key.
  val (==) : (ctx, key) -> (ctx, key) -> bool

  -- | A given hash function use.
  val hash : ctx -> [m]uint -> key -> uint
}
