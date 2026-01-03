import "../../lib/github.com/diku-dk/containers/hyperloglog"
import "../../lib/github.com/diku-dk/containers/key"
import "../../lib/github.com/diku-dk/containers/hashkey"

module i64key_simple_u32 : hashkey with key = i64 with hash = u32 with ctx = () = {
  type~ ctx = ()

  type key = i64

  type const = ()

  type hash = u32

  type rng = ()

  def rng_from_seed _ = ()

  def rand () = ((), ())

  def (==) ((), k) ((), k') = k i64.== k'

  def hash () () k = u32.i64 k
}

module i64key_simple : hashkey with key = i64 with hash = u64 with ctx = () = {
  type~ ctx = ()

  type key = i64

  type const = ()

  type hash = u64

  type rng = ()

  def rng_from_seed _ = ()

  def rand () = ((), ())

  def (==) ((), k) ((), k') = k i64.== k'

  def hash () () k = u64.i64 k
}

module hll = mk_hyperloglog i64key_u32
module hllpp = mk_hyperloglog_plusplus i64key

def hll_plot (s: i64) (n : i64) : ([]f64, []f64) =
  let h = hll.create 12
  let xs = replicate n 0.0
  let ys = replicate n 0.0
  let (xs, ys, _) =
      loop (xs, ys, h)
      for i < n do
        let h = hll.insert () h (i*s..<(i + 1)*s)
        let xs[i] = f64.i64 <| (i + 1) * s
        let ys[i] = f64.abs (hll.count h - xs[i]) / xs[i]
        in (xs, ys, h)
  in (xs, ys)

def hllpp_plot (s: i64) (n : i64) : ([]f64, []f64) =
  let h = hllpp.create 12
  let xs = replicate n 0.0
  let ys = replicate n 0.0
  let (xs, ys, _) =
      loop (xs, ys, h)
      for i < n do
        let h = hllpp.insert () h (i*s..<(i + 1)*s)
        let xs[i] = f64.i64 <| (i + 1) * s
        let ys[i] = f64.abs (hllpp.count h - xs[i]) / xs[i]
        in (xs, ys, h)
  in (xs, ys)


-- > :plot2d {hll=hll_plot 10i64 10000i64, hllpp=hllpp_plot 10i64 10000i64}

-- > :plot2d {hll=hll_plot 1000i64 10000i64, hllpp=hllpp_plot 1000i64 10000i64}

-- > :plot2d {hll=hll_plot 1000i64 1000000i64, hllpp=hllpp_plot 1000i64 1000000i64} 