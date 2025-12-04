def height (n: i64) =
  let temp = i64.num_bits - i64.clz n
  in i64.i32 <| if i64.popc n == 1 then temp else temp + 1

def size (n: i64) =
  (1 << (height n)) - 1

def sibling (i: i64) =
  i - i64.bool (i % 2 == 0) + i64.bool (i % 2 == 1)

def parent (i: i64) =
  (i - 1) / 2i64

def is_left (i: i64) =
  i % 2 == 1i64

def is_right (i: i64) =
  i % 2 == 0i64

module type transparent_prefix_tree = {
  type tree [n] 't
  val make [n] 't : (op: t -> t -> t) -> (ne: t) -> (arr: [n]t) -> tree [size n] t
  val previous [n] 't : (op: t -> t -> bool) -> (tree: tree [size n] t) -> (i: i64) -> i64
  val next [n] 't : (op: t -> t -> bool) -> (tree: tree [size n] t) -> (i: i64) -> i64
}

module transparent_prefix_tree : transparent_prefix_tree with tree [n] 't = [n]t = {
  type tree [n] 't = [n]t

  def make [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t) : tree [size n] t =
    let h = height n
    let tree_size = size n
    let offset = size (h - 1)
    let offsets = iota n |> map (+ offset)
    let tree = scatter (replicate tree_size ne) offsets arr
    let arr = copy tree[offset:]
    let (tree, _, _) =
      loop (tree, arr, level) = (tree, arr, h - 2)
      while level >= 0 do
        let new_size = length arr / 2
        let new_arr =
          tabulate new_size (\i -> arr[2 * i] `op` arr[2 * i + 1])
        let offset = size level
        let offsets = iota new_size |> map (+ offset)
        let new_tree = scatter tree offsets new_arr
        in (new_tree, new_arr, level - 1)
    in tree

  def previous [n] 't
               (op: t -> t -> bool)
               (tree: tree [size n] t)
               (i: i64) : i64 =
    if i < 0 || n <= i
    then -1
    else let h = i64.i32 <| i64.num_bits - i64.clz (size n)
         let offset = size (h - 1)
         let start = offset + i
         let v = tree[start]
         let ascent j = j != 0 && (is_left j || !(tree[sibling j] `op` v))
         let descent j = 2 * j + 1 + i64.bool (tree[2 * j + 2] `op` v)
         let index = iterate_while ascent parent start
         in if index != 0
            then iterate_while (< offset) descent (sibling index) - offset
            else -1

  def next [n] 't
           (op: t -> t -> bool)
           (tree: tree [size n] t)
           (i: i64) : i64 =
    if i < 0 || n <= i
    then -1
    else let h = i64.i32 <| i64.num_bits - i64.clz (size n)
         let offset = size (h - 1)
         let start = offset + i
         let v = tree[start]
         let ascent j = j != 0 && (is_right j || !(tree[sibling j] `op` v))
         let descent j = 2 * j + 2 - i64.bool (tree[2 * j + 1] `op` v)
         let index = iterate_while ascent parent start
         in if index != 0
            then iterate_while (< offset) descent (sibling index) - offset
            else -1
}
