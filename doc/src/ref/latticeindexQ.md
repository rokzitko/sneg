---
keywords: lattice index test, predicate
see also: momentumindexQ, latticeindextype, sneglatticeindexes, latticesize
---
# latticeindexQ

## Usage
- `latticeindexQ[R]` gives `True` if *R* is a symbol declared as a lattice index with `sneglatticeindexes`, and `False` otherwise.

## Details
- `latticeindexQ[R]` is `latticeindextype[R] === "Lattice"`. Lattice vectors (`sneglatticevectors`), momentum indexes, and expressions such as `R + 1` give `False`.

## Examples
### Basic Examples
```wl
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1];
{latticeindexQ[R], latticeindexQ[k], latticeindexQ[a1], latticeindexQ[R + 1]}
```
