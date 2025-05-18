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

  -- | Number constants the hash function is depended on.
  val m : i64

  -- | Equality definition for the key.
  val eq : ctx -> key -> ctx -> key -> bool

  -- | A given hash function use.
  val hash : ctx -> [m]u64 -> key -> u64
}

-- | Hashable 64-bit integers.
module i64_key : hashkey with ctx = () with key = i64 = {
  type key = i64
  type ctx = ()

  def m : i64 = 1

  -- The hash function was found [here](http://stackoverflow.com/a/12996028).
  def hash _ (a: [m]u64) (x: i64) : u64 =
    let x = a[0] * u64.i64 x
    let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
    let x = (x ^ (x >> 27)) * 0x94d049bb133111eb
    let y = (x ^ (x >> 31))
    in y

  def eq _ x _ y = x i64.== y
}
