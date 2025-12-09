import "../../lib/github.com/diku-dk/containers/unionfind"

-- Start with some utility definitions for handling directions and positions.

-- | A cardinal direction, with `#c` being current location ("centre").
type dir = #n | #w | #e | #s

-- | Position in a grid.
type pos = (i64, i64)

-- | Move along direction.
def move (d: dir) ((i, j): pos) =
  match d
  case #n -> (i - 1, j)
  case #w -> (i, j - 1)
  case #e -> (i, j + 1)
  case #s -> (i + 1, j)

-- | Turn a position into a flat index, given a grid width.
def flat_pos (w: i64) ((x, y): pos) : i64 = x * w + y

-- | Is this position in bounds in some grid?
def in_bounds [h] [w] 'a (_: [h][w]a) ((i, j): pos) =
  i >= 0 && i < h && j >= 0 && j < w

-- | Get element at position in grid.
def get 'a ((i, j): pos) (g: [][]a) =
  g[i, j]

-- | Could be improved. This is unlikely to produce something very legible.
def colourise_regions [h] [w] (labels: [h][w]i64) : [h][w]u32 =
  let f l = u32.i64 l
  in map (map f) labels

def mk_nasty (w: i64) : [w][w]u32 =
  tabulate_2d w w \i j ->
    if (j % 2 == 0) || (i == 0 && (j / 2) % 2 == 0)
    || (i == w - 1 && (j / 2) % 2 == 1)
    then 0
    else 0xffffff

def mk_equivalences [h] [w] (img: [h][w]u32) : ?[n].[n](i64, i64) =
  tabulate_2d h
              w
              (\i j ->
                 let p = (i, j)
                 let flat_p = flat_pos w p
                 in map (\n ->
                           let p' = move n p
                           in if in_bounds img p' && get p img == get p' img
                              then (flat_p, flat_pos w p')
                              else (flat_p, -1))
                        [#n, #w, #e, #s])
  |> flatten_3d
  |> filter ((>= 0) <-< (.1))

module u = unionfind

def region_label_unionfind [h] [w] (img: [h][w]u32) =
  let uf = u.create (h * w)
  let eqs =
    copy (mk_equivalences img
          |> map (\(i, j) ->
                    ( u.from_i64 uf i
                    , u.from_i64 uf j
                    )))
  let uf = u.union uf eqs
  let labels = u.find' uf (u.handles uf)
  in unflatten (map (u.to_i64 uf) labels :> [h * w]i64)

-- > :img (colourise_regions (region_label_unionfind ($loadimg "regions-hard.png")))

-- > :img mk_nasty 128

-- > :img colourise_regions (region_label_unionfind (mk_nasty 128))
