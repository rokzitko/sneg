---
keywords: number of unit cells, lattice size, number of k points
see also: sneglatticeindexes, snegmomentumindexes, latticeindexQ, momentumindexQ, latticeindextype
---
# latticesize

## Usage
- `latticesize[k]` gives the number of unit cells $N_s$ of the lattice of a declared lattice or momentum index *k*.

## Details
- The value is the second argument of `sneglatticeindexes` or `snegmomentumindexes`. For other symbols, `latticesize[k]` remains unevaluated.
- $N_s$ is the number of values of the index: `sum[1, {k}]` is `latticesize[k]`.

## Examples
### Basic Examples
```wl
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, 8];
{latticesize[R], latticesize[k], latticesize[x]}
```
