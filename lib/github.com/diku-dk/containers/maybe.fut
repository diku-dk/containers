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

-- | Definition of a `maybe`@term equality.
-- 
-- The equality holds if they are both `#nothing`@term or they are both
-- `#just` and the values inside `#just` are equal.
def equal_maybe 'a (eq : a -> a -> bool) : maybe a -> maybe a -> bool =
  (\a b ->
    match (a, b)
    case (#just a', #just b') -> a' `eq` b'
    case (#nothing, #nothing) -> true
    case _ -> false
  )

-- | Maps a value to a `maybe`@term type.
-- 
-- This is just syntactic sugar for `#just a`@term, it may be nicer
-- to use then a lambda function.
def to_just 'a (a : a) : maybe a =
  #just a

-- | Predicate for determing if a maybe type is the `#just`@term constructor.
-- 
-- Holds true if `a`@term is `#just`@term.
def is_just 'a (a : maybe a) : bool =
  match a
  case #just _ -> true
  case _ -> false

-- | Predicate for determing if a maybe type is the `#nothing`@term constructor.
-- 
-- Holds true if `a`@term is `#nothing`@term.
def is_nothing 'a (a : maybe a) : bool =
  is_just a
  |> not