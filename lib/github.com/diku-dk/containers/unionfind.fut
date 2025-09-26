import "hashkey"
import "hashmap"
import "opt"

module type unionfind = {
  type t
  type~ unionfind [n]
  type~ ctx
  val from_array [n] : ctx -> [n]t -> unionfind [n]
  val union [n] : unionfind [n] -> [n](t, t) -> unionfind [n]
  val find [n] : unionfind [n] -> t -> t
}

module mk_unionfind (K: hashkey with hash = u64) = {
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

  def find_idx [n] (uf: unionfind [n]) (t: t) : (i64, i64) =
    let ctx = hashmap.context uf.mapping
    let j = hashmap.lookup ctx t uf.mapping |> from_opt (-1)
    in if 0 <= j && j < n
       then (\k -> (k, j))
            <| loop i = j
               while uf.elems[i].0 != -1 do
                 uf.elems[i].0
       else (-1, -1)

  def find [n] (uf: unionfind [n]) (t: t) : opt t =
    let (i, _) = find_idx uf t
    in if 0 <= i && i < n
       then #some uf.elems[i].1
       else #none

  def union [n] (uf: unionfind [n]) (eqs: [n](t, t)) : unionfind [n] =
    let (ps, is) =
      map (\(t, t') ->
             let (i, j) = find_idx uf t
             let (i', j') = find_idx uf t'
             in if i < i' then (i, j') else (i', j))
          eqs
      |> unzip
    let (parents, ts) = unzip uf.elems
    let parents' = scatter (copy parents) is ps
    in uf with elems = zip parents' ts
}
