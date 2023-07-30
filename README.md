# A Futhark package with Containers

A library which contains useful generic data structures.

## Installation

```
$ futhark pkg add github.com/diku-dk/containers
$ futhark pkg sync
```

## Usage
```
> import "lib/github.com/diku-dk/containers/bitset"
> module bitset_u8 = mk_bitset u8
> let capacity = i64.i32 6
> let set = bitset_u8.from_array capacity [0, 1, 2]
> bitset_u8.is_empty set
```
