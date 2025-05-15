import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/containers/key"
import "lib/github.com/diku-dk/containers/hashmap"
import "lib/github.com/diku-dk/containers/hashset"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/containers/array"

module type slice = {
  type elem
  type slice
  val mk : (i: i64) -> (n: i64) -> slice
  val unmk : slice -> (i64, i64)
  val get [n] : slice -> [n]elem -> ?[k].[k]elem
}

module mk_slice (E: {type elem}) : slice with elem = E.elem = {
  type elem = E.elem
  type slice = {i: i64, n: i64}
  def mk i n = {i, n}
  def unmk {i, n} = (i, n)
  def get {i, n} xs = xs[i:i + n]
}

def arreq [n] [m] 't (eq: t -> t -> bool) (x: [n]t) (y: [m]t) =
  n == m
  && (loop (ok, i) = (true, 0)
      while ok && i < n do
        (ok && (x[i] `eq` y[i]), i + 1)).0

module mk_slice_key
  (S: slice)
  (E: {
    val (==) : S.elem -> S.elem -> bool
    val word : S.elem -> u64
  })
  : key
    with ctx = []S.elem
    with i = u64
    with k = S.slice = {
  type i = u64
  type k = S.slice
  type~ ctx = ?[l].[l]S.elem

  def m : i64 = 1

  def eq (ctx: []S.elem) (x: k) (y: k) =
    arreq (E.==) (S.get x ctx) (S.get y ctx)

  def hash (ctx: []S.elem) (a: [m]u64) (x: k) : u64 =
    loop v = 0
    for x' in S.get x ctx do
      let x = a[0] * (v ^ E.word x')
      let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9
      let x = (x ^ (x >> 27)) * 0x94d049bb133111eb
      let y = (x ^ (x >> 31))
      in y
}

module engine = xorshift128plus
module u8slice = mk_slice {type elem = u8}

module slice_key = mk_slice_key u8slice {
  def (==) = (u8.==)
  def word = u64.u8
}

module array = mk_array slice_key engine
module hashset = mk_hashset slice_key engine

def seed = engine.rng_from_seed [1]

type char = u8

def is_space (x: char) = x == ' '
def isnt_space x = !(is_space x)

def (&&&) f g x = (f x, g x)

def words [n] (s: [n]char) =
  segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
  |> (id &&& rotate 1)
  |> uncurry zip
  |> zip (indices s)
  |> filter (\(i, (x, y)) -> (i == n - 1 && x > 0) || x > y)
  |> map (\(i, (x, _)) -> (i - x + 1, x))

entry mkinput (s: []char) = (s, words s)

entry bench_hashset_dedup [l] [k] (ctx: [l]u8) (words: [k](i64, i64)) =
  hashset.from_array ctx seed (map (uncurry u8slice.mk) words)
  |> (.1)
  |> hashset.to_array
  |> map u8slice.unmk
  |> map (.0)

entry bench_array_dedup [l] [k] (ctx: [l]u8) (words: [k](i64, i64)) =
  array.dedup ctx seed (map (uncurry u8slice.mk) words)
  |> (.1)
  |> map u8slice.unmk
  |> map (.0)

entry bench_sort_dedup [l] [k] (ctx: [l]u8) (words: [k](i64, i64)) =
  let longest_word = i32.maximum (map (i32.i64 <-< (.1)) words)
  let get_bit j (i, n) =
    if j < i32.i64 n * 8
    then u8.get_bit (j % 8) ctx[i + i64.i32 j / 8]
    else 0
  let sorted = radix_sort (longest_word * 8) get_bit words
  let flags =
    map (\i ->
           i == 0
           || !arreq (==)
                     (u8slice.get (uncurry u8slice.mk sorted[i - 1]) ctx)
                     (u8slice.get (uncurry u8slice.mk sorted[i]) ctx))
        (indices words)
  in zip flags sorted |> filter (.0) |> map (.1.0)

-- ==
-- entry: bench_hashset_dedup bench_sort_dedup bench_array_dedup
-- script input { mkinput ($loadbytes "words.txt") }
