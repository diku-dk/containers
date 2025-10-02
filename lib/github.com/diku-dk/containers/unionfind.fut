import "hashkey"
import "hashmap"
import "opt"

module type unionfind = {
  type t
  type~ unionfind [n]
  type~ ctx
  val from_array [n] : ctx -> [n]t -> unionfind [n]
  val union [n] [u] : unionfind [n] -> [u](t, t) -> unionfind [n]
  val find [n] : unionfind [n] -> t -> opt t
}

module mk_unionfind (K: hashkey with hash = u64)
  : unionfind
    with ctx = K.ctx
    with t = K.key = {
  module hashmap = mk_hashmap K

  type t = K.key
  type~ ctx = K.ctx

  type~ unionfind [n] =
    { elems: [n](i64, t)
    , mapping: hashmap.map [n] i64
    }

  def from_array [n] (ctx: ctx) (ts: [n]t) : unionfind [n] =
    let es = zip (rep (-1i64)) ts
    let kvs = zip ts (iota n)
    let hs = hashmap.from_array_nodup ctx kvs
    in {elems = es, mapping = hs}

  def find_index [n] (uf: unionfind [n]) (t: t) : i64 =
    let ctx = hashmap.context uf.mapping
    in hashmap.lookup ctx t uf.mapping |> from_opt (-1)

  def find_parent [n] (uf: unionfind [n]) (t: t) : i64 =
    let j = find_index uf t
    in loop i = j
       while uf.elems[i].0 != -1 do
         uf.elems[i].0

  def find [n] (uf: unionfind [n]) (t: t) : opt t =
    let i = find_index uf t
    in if 0 <= i && i < n
       then #some uf.elems[i].1
       else #none

  def loop_body [n] [u] (uf: unionfind [n]) (eqs: [u](i64, i64)) : ?[m].(unionfind [n], [m](i64, i64)) =
    let ps = map (.1) eqs
    let parents = hist i64.min i64.highest n ps (iota u)
    let eqs' =
      zip (iota u) eqs
      |> filter (\(i, (_, p)) -> parents[i] != p)
      |> map (.1)
    let (ps', ts) = unzip uf.elems
    let ps'' =
      map (\p -> if 0 <= p && p < u then ps[p] else -1) parents
      |> scatter (copy ps') parents
    in (uf with elems = zip ps'' ts, eqs')

  def union [n] [u] (uf: unionfind [n]) (eqs: [u](t, t)) : unionfind [n] =
    let both f (a, b) = (f a, f b)
    let eqs' = map (both (find_index uf)) eqs
    let (uf', _) =
      loop (uf', eqs') = (uf, eqs')
      while length eqs' != 0 do
        loop_body uf' eqs'
    in uf'
}

module mk_unionfind_sequential (K: hashkey with hash = u64)
  : unionfind
    with ctx = K.ctx
    with t = K.key = {
  module hashmap = mk_hashmap K

  type t = K.key
  type~ ctx = K.ctx

  type~ unionfind [n] =
    { elems: [n](i64, t)
    , mapping: hashmap.map [n] i64
    }

  def from_array [n] (ctx: ctx) (ts: [n]t) : unionfind [n] =
    let es = zip (rep (-1i64)) ts
    let kvs = zip ts (iota n)
    let hs = hashmap.from_array_nodup ctx kvs
    in {elems = es, mapping = hs}

  def find_index [n] (uf: unionfind [n]) (t: t) : i64 =
    let ctx = hashmap.context uf.mapping
    in hashmap.lookup ctx t uf.mapping |> from_opt (-1)

  def find_parent [n] (uf: unionfind [n]) (t: t) : i64 =
    let j = find_index uf t
    in loop i = j
       while uf.elems[i].0 != -1 do
         uf.elems[i].0

  def find [n] (uf: unionfind [n]) (t: t) : opt t =
    let i = find_index uf t
    in if 0 <= i && i < n
       then #some uf.elems[i].1
       else #none

  def union [n] [u] (uf: unionfind [n]) (eqs: [u](t, t)) : unionfind [n] =
    loop uf' = copy uf
    for (t, t') in eqs do
      let i = find_index uf' t
      let p = find_index uf' t'
      let (_, t) = uf'.elems[i]
      in uf' with elems = (uf'.elems with [i] = (p, copy t))
}
