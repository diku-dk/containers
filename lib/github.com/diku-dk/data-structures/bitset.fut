module type bitset = {
  -- | The integer type.
  type t
  -- | The bitset type.
  type bitset[n]
  -- | The number of bits for the chosen integral type.
  val nbs : i64
  -- | Makes a empty bitset of a given capacity.
  val empty : (n : i64) -> bitset[(n - 1) / nbs + 1]
  -- | Makes a singleton bitset with a given capacity.
  val singleton : (n : i64) -> i64 -> bitset[(n - 1) / nbs + 1]
  -- | Checks if a bitset is empty.
  val is_empty [n] : bitset[(n - 1) / nbs + 1] -> bool
  -- | Inserts a single bit in a bitset.
  val insert [n] : i64 -> bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1]
  -- | Deletes a single bit in a bitset.
  val delete [n] : i64 -> bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1]
  -- | Checks if a bit is a member of a bitset.
  val member [n] : i64 -> bitset[(n - 1) / nbs + 1] -> bool
  -- | Bitset union.
  val union [n] : bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1]
  -- | Bitset intersection.
  val intersection [n] : bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1]
  -- | Bitset difference.
  val difference [n] : bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1]
  -- | Checks if a bitset is a subset of another.
  val is_subset [n] : bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1] -> bool
  -- | Finds the complement of a bitset.
  val complement [n] : bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1]
  -- | Sets the bitset capacity to a new value.
  val set_capacity [m] : (n : i64) -> bitset[(m - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1]
  -- | Computes the size of the set i.e. the population count.
  val size [n] : bitset[(n - 1) / nbs + 1] -> i64
  -- | If a two bitsets contains the same bits then they are equal.
  val == [n] : bitset[(n - 1) / nbs + 1] -> bitset[(n - 1) / nbs + 1] -> bool
  -- | Convert an array of indices to a bitset.
  val from_array [m] : (n : i64) -> [m]i64 -> bitset[(n - 1) / nbs + 1]
  -- | Converts an array of integral types to a bitset.
  val from_bit_array [m] : (n : i64) -> (arr : [m]t) -> bitset[(n - 1) / nbs + 1]
  -- | Convert a bitset to an array of indices to a bitset.
  val to_array [n] : bitset[(n - 1) / nbs + 1] -> []i64
}

module mk_bitset (I: integral) : bitset = {
  def nbs = i64.i32 I.num_bits

  type t = I.t
  type bitset [n] = [n]t
  type maybe 'a = #just a | #nothing 

  def zero : t = I.u64 0
  
  def empty (n : i64) : bitset[(n - 1) / nbs + 1] =
    replicate ((n - 1) / nbs + 1) zero
  
  def find_bitset_index (i : i64) (n : i64) : maybe (i64, i32) =
    if i < 0 || n <= i
    then #nothing
    else let nbs = i64.i32 I.num_bits
         let j = i / nbs
         let bit = i % nbs
         in #just (j, i32.i64 bit)

  def set_bit [n] ((i, bit) : (i64, i32)) (s : bitset[(n - 1) / nbs + 1]) (value : i32) =
    copy s with [i] = I.set_bit bit s[i] value

  def insert [n] (i : i64) (s : bitset[(n - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    match find_bitset_index i n
    case #just index -> set_bit index s 1
    case #nothing -> s

  def singleton (n : i64) (i : i64) : bitset[(n - 1) / nbs + 1] =
    empty n
    |> insert i

  def is_empty [n] (s : bitset[(n - 1) / nbs + 1]) : bool =
    all (I.==zero) s
  
  def delete [n] (i : i64) (s : bitset[(n - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    match find_bitset_index i n
    case #just index -> set_bit index s 0
    case #nothing -> s

  def member [n] (i : i64) (s : bitset[(n - 1) / nbs + 1]) : bool =
    match find_bitset_index i n
    case #just (i, bit) -> I.get_bit bit s[i] == 1
    case #nothing -> false

  def union [n] (a : bitset[(n - 1) / nbs + 1]) (b : bitset[(n - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    map2 (I.|) a b
  
  def intersection [n] (a : bitset[(n - 1) / nbs + 1]) (b : bitset[(n - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    map2 (I.&) a b

  def set_leading_bits_zero [n] (s : bitset[(n - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    let l = (n - 1) / nbs + 1
    let start = 1 + (n - 1) % nbs
    let to_keep = I.i64 (i64.not (i64.not 0 << start))
    in if l == 0
       then s
       else copy s with [l - 1] = s[l - 1] I.& to_keep
  
  def complement [n] (s : bitset[(n - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    map I.not s
    |> set_leading_bits_zero
  
  def size [n] (s : bitset[(n - 1) / nbs + 1]) : i64 =
    map (i64.i32 <-< I.popc) s
    |> i64.sum

  def (==) [n] (a : bitset[(n - 1) / nbs + 1]) (b : bitset[(n - 1) / nbs + 1]) : bool =
    map2 (I.==) a b
    |> and

  def is_subset [n] (a : bitset[(n - 1) / nbs + 1]) (b : bitset[(n - 1) / nbs + 1]) : bool =
    (a `union` b) == b
  
  def difference [n] (a : bitset[(n - 1) / nbs + 1]) (b : bitset[(n - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    a `intersection` complement b
  
  def set_capacity [m] (n : i64) (s : bitset[(m - 1) / nbs + 1]) : bitset[(n - 1) / nbs + 1] =
    let s' = empty n
    let len = length s
    in map (\i ->
         if i < len then s[i] else zero
       ) (indices s')
       |> set_leading_bits_zero

  def from_bit_array [m] (n : i64) (arr : [m]t) : bitset[(n - 1) / nbs + 1] =
    sized ((n - 1) / nbs + 1) arr
    |> set_leading_bits_zero

  -- There is probably a way to do this more space efficient.
  def from_array [m] (n : i64) (arr : [m]i64) : bitset[(n - 1) / nbs + 1] =
    let empty' = empty n
    in map (singleton n) arr
       |> reduce_comm union empty'
  
  def to_array [n] (s : bitset[(n - 1) / nbs + 1]) : []i64 =
    map2 (\i v ->
      let m = i * i64.i32 I.num_bits
      in map (\bit ->
         if I.get_bit (i32.i64 bit) v i32.== 1
         then m + bit
         else -1
      ) (iota (i64.i32 I.num_bits))
    ) (indices s) s
    |> flatten
    |> filter (0<=)
}