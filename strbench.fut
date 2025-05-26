import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/containers/hashkey"
import "lib/github.com/diku-dk/containers/key"
import "lib/github.com/diku-dk/containers/hashmap"
import "lib/github.com/diku-dk/containers/hashset"
import "lib/github.com/diku-dk/cpprandom/random"
import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/containers/array"
import "lib/github.com/diku-dk/containers/slice"

module engine = xorshift128plus

module slice_key = mk_slice_key u8key

module array_key = mk_array_key slice_key engine
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
  hashset.from_array ctx (map (uncurry slice.mk) words)
  |> hashset.to_array
  |> map slice.unmk
  |> map (.0)

entry bench_array_dedup [l] [k] (ctx: [l]u8) (words: [k](i64, i64)) =
  array_key.dedup ctx seed (map (uncurry slice.mk) words)
  |> (.1)
  |> map slice.unmk
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
           || !array.eq (==)
                        (slice.get (uncurry slice.mk sorted[i - 1]) ctx)
                        (slice.get (uncurry slice.mk sorted[i]) ctx))
        (indices words)
  in zip flags sorted |> filter (.0) |> map (.1.0)

-- ==
-- entry: bench_hashset_dedup bench_sort_dedup bench_array_dedup
-- script input { mkinput ($loadbytes "words.txt") }
