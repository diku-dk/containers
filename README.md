# containers [![CI](https://github.com/diku-dk/containers/workflows/CI/badge.svg)](https://github.com/diku-dk/containers/actions) [![Documentation](https://futhark-lang.org/pkgs/github.com/diku-dk/containers/status.svg)](https://futhark-lang.org/pkgs/github.com/diku-dk/containers/latest/)

A Futhark library of generic data structures.

## Installation

```
$ futhark pkg add github.com/diku-dk/containers
$ futhark pkg sync
```

## Usage
```
[0]> import "lib/github.com/diku-dk/containers/bitset"
[1]> let capacity = i64.i32 6
[2]> let set = bitset.from_array capacity [0, 1, 2]
[3]> bitset.is_empty set
false
```
