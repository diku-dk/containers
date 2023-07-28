module type bitset = {
  -- | The bitset type.
  type bitset[n]
  val num_bits : i64
  -- | Makes a empty bitset of a given capacity.
  val empty : (capacity : i64) -> bitset[(capacity - 1) / num_bits + 1]
  -- | Makes a singleton bitset with a given capacity.
  val singleton : (capacity : i64) -> i64 -> bitset[(capacity - 1) / num_bits + 1]
  -- | Checks if a bitset is empty.
  val is_empty [n] : bitset[n] -> bool
  -- | Inserts a single bit in a bitset.
  val insert [n] : i64 -> bitset[n] -> bitset[n]
  -- | Deletes a single bit in a bitset.
  val delete [n] : i64 -> bitset[n] -> bitset[n]
  -- | Checks if a bit is a member of a bitset.
  val member [n] : i64 -> bitset[n] -> bool
  -- | Bitset union.
  val union [n] : bitset[n] -> bitset[n] -> bitset[n]
  -- | Bitset intersection.
  val intersection [n] : bitset[n] -> bitset[n] -> bitset[n]
  -- | Bitset difference.
  val difference [n] : bitset[n] -> bitset[n] -> bitset[n]
  -- | Checks if a bitset is a subset of another.
  val is_subset [n] : bitset[n] -> bitset[n] -> bool
  -- | Finds the complement of a bitset.
  val complement [n] : bitset[n] -> bitset[n]
  -- | Sets the bitset capacity to a new value.
  -- val set_capacity [n] : (capacity : i64) -> bitset[n] -> bitset[(capacity - 1) / num_bits + 1]
  -- | Computes the size of the set i.e. the population count.
  val size [n] : bitset[n] -> i64
  -- | If a two bitsets contains the same bits then they are equal.
  val == [n] : bitset[n] -> bitset[n] -> bool
  -- | Convert an array of indices to a bitset.
  val from_array [n] : (capacity : i64) -> [n]i64 -> bitset[(capacity - 1) / num_bits + 1]
  -- | Convert a bitset to an array of indices to a bitset.
  val to_array [n] : bitset[n] -> []i64
}

module mk_bitset (I: integral) : bitset = {
  type int = I.t
  type bitset [n] = {bits: [n]int, capacity: i64}
  type maybe 'a = #just a | #nothing 

  def num_bits = i64.i32 I.num_bits
  def zero : int = I.i64 0

  def empty (n : i64) : bitset[] =
    let len = (n - 1) / num_bits + 1
    in {bits=sized len <| replicate len zero, capacity=n}
  
  def find_bitset_index (i : i64) (capacity : i64) : maybe (i64, i32) =
    if i < 0 || capacity <= i
    then #nothing
    else let num_bits = i64.i32 I.num_bits
         let j = i / num_bits
         let bit = i % num_bits
         in #just (j, i32.i64 bit)

  def set_bit [n] ((i, bit) : (i64, i32)) (bits : [n]int) (value : i32) =
    copy bits with [i] = I.set_bit bit bits[i] value

  def insert [n] (i : i64) (s : bitset[n]) : bitset[n] =
    match find_bitset_index i s.capacity
    case #just index -> s with bits = set_bit index s.bits 1
    case #nothing -> s

  def singleton (capacity : i64) (i : i64) : bitset[] =
    empty capacity
    |> insert i

  def is_empty [n] (s : bitset[n]) : bool =
    all (I.==zero) s.bits
  
  def delete [n] (i : i64) (s : bitset[n]) : bitset[n] =
    match find_bitset_index i s.capacity
    case #just index -> s with bits = set_bit index s.bits 0
    case #nothing -> s

  def member [n] (i : i64) (s : bitset[n]) : bool =
    match find_bitset_index i s.capacity
    case #just (i, bit) -> I.get_bit bit s.bits[i] == 1
    case #nothing -> false

  def union [n] (a : bitset[n]) (b : bitset[n]) : bitset[n] =
    if a.capacity != b.capacity
    then assert false a
    else {bits=map2 (I.|) a.bits b.bits, capacity=a.capacity}
  
  def intersection [n] (a : bitset[n]) (b : bitset[n]) : bitset[n] =
    if a.capacity != b.capacity
    then assert false a
    else {bits=map2 (I.&) a.bits b.bits, capacity=a.capacity}

  def set_front_zero (start : i32) (value : int) : int =
    loop v = value for i in (start...I.num_bits - 1) do
      I.set_bit i v 0
  
  def set_front_zero_bits [n] (capacity : i64) (bits : [n]int) : [n]int =
    let start = i32.i64 <| 1 + ((capacity - 1) % num_bits)
    in if capacity == 0 || start >= I.num_bits
       then bits
       else copy bits with [n - 1] = set_front_zero start bits[n - 1]

  def set_front_bits_zero [n] (s : bitset[n]) : bitset[n] =
    s with bits = set_front_zero_bits s.capacity s.bits
  
  def complement [n] (s : bitset[n]) : bitset[n] =
    {bits=map I.not s.bits, capacity=s.capacity}
    |> set_front_bits_zero
  
  def size [n] (s : bitset[n]) : i64 =
    map (i64.i32 <-< I.popc) s.bits
    |> i64.sum

  def (==) [n] (a : bitset[n]) (b : bitset[n]) : bool =
    map2 (I.==) a.bits b.bits
    |> and

  def is_subset [n] (a : bitset[n]) (b : bitset[n]) : bool =
    (a `union` b) == b
  
  def difference [n] (a : bitset[n]) (b : bitset[n]) : bitset[n] =
    a `intersection` complement b
  
  def set_capacity [m] (capacity : i64) (s : bitset[m]) : bitset[(capacity - 1) / num_bits + 1] =
    let s' = empty capacity
    let len = length s.bits
    in {bits=
          map (\i ->
            if i < len then s.bits[i] else zero
          ) (indices s'.bits), 
        capacity=capacity}
        |> set_front_bits_zero

  -- There is probably a way to do this more space efficient.
  def from_array [n] (capacity : i64) (arr : [n]i64) : bitset[(capacity - 1) / num_bits + 1] =
    let empty' = empty capacity
    in map (singleton capacity) arr
       |> reduce_comm union empty'
  
  def to_array [n] (a : bitset[n]) : []i64 =
    map2 (\i v ->
      let m = i * i64.i32 I.num_bits
      in map (\bit ->
         if I.get_bit (i32.i64 bit) v i32.== 1
         then m + bit
         else -1
      ) (iota (i64.i32 I.num_bits))
    ) (indices a.bits) a.bits
    |> flatten
    |> filter (0<=)
}