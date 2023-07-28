# Bitset package for Futhark

A small library for performing bitset operations.

## Installation

```
$ futhark pkg add github.com/WilliamDue/bitset
$ futhark pkg sync
```

## Usage
```
> import "lib/github.com/WilliamDue/bitset/bitset"
> module bitset_u8 = mk_bitset u8
> let capacity = i64.i32 6
> let a_set = bitset_u8.from_array capacity [0, 1, 2]
> bitset_u8.is_empty [0, 1, 2]
```
