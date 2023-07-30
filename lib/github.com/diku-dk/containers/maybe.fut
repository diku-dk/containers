-- | `maybe`@term type.
--
-- A type which can be used to signify the lack of a value.

-- | The `maybe`@term type
--
-- A `maybe`@term type is a sum type. It is either `#just`@term containg a 
-- value `a`@term or `#nothing`@term.
type maybe 'a = #just a | #nothing  

-- | Extents a binary operation to have `#nothing`@term as a identity element. 
--
-- This can be used when a associative operation has [no neutral element](https://futhark-lang.org/examples/no-neutral-element.html).
def add_identity 'a (op : a -> a -> a) : maybe a -> maybe a -> maybe a =
  (\a b ->
    match (a, b)
    case (#just a', #just b') -> #just (a' `op` b')
    case (#just _, #nothing) -> a
    case (#nothing, #just _) -> b
    case (#nothing, #nothing) -> #nothing
  )

-- | Unpacks a `maybe`@term type.
-- 
-- If `#just a`@term then `a` is returned, otherwise `ne`@term is returned.
def from_maybe 'a (a : maybe a) (ne : a) : a =
  match a
  case #just a' -> a'
  case #nothing -> ne

-- | Maps a function inside `maybe`@term.
-- 
-- Applies the function `f`@term to the value `'a`.
def map_maybe 'a 'b (f : a -> b) (a : maybe a) : maybe b =
  match a
  case #just a' -> #just (f a')
  case #nothing -> #nothing