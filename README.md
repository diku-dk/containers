# A Futhark package with Data Structures

A library which contains useful data structures.

## Installation

```
$ futhark pkg add github.com/diku-dk/data-structures
$ futhark pkg sync
```

## Usage
```
> import "lib/github.com/diku-dk/data-structures/bitset"
> module bitset_u8 = mk_bitset u8
> let capacity = i64.i32 6
> let set = bitset_u8.from_array capacity [0, 1, 2]
> bitset_u8.is_empty set
```
