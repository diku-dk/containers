import "hashkey"
import "hashmap"
import "opt"

module type unionfind = {
  type t
  type~ unionfind [n]
  val from_array [n] : [n]t -> unionfind [n]
  val merge [n] : unionfind [n] -> [n](t, t) -> unionfind [n]
  val find [n] : unionfind [n] -> t -> t
}

module mk_unionfind (K: hashkey with hash = u64 with ctx = ()) = {
  module hashmap = mk_hashmap K

  type t = K.key

  type elem =
    { parent: i64
    , elem: t
    }

  type~ unionfind [n] =
    { elems: [n]elem
    , mapping: hashmap.map [n] i64
    }

  def from_array [n] (ts: [n]t) : unionfind [n] =
    let es = map (\t -> {parent = -1i64, elem = t}) ts
    let kvs = zip ts (iota n)
    let hs = hashmap.from_array_nodup () kvs
    in {elems = es, mapping = hs}

  def find [n] (uf: unionfind [n]) (t: t) : opt t =
    let j = from_opt (-1) (hashmap.lookup () t uf.mapping)
    in if 0 <= j && j < n
       then let idx =
              loop i = j
              while uf.elems[i].parent != -1 do
                uf.elems[i].parent
            in #some uf.elems[idx].elem
       else #none
}
