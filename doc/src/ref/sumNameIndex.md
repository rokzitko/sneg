---
keywords: dummy index, abstract index, rename summation index, symbolic sum
see also: sumAbstractIndex, sumCollect, sum
---
# sumNameIndex

## Usage
- `sumNameIndex[expr, idx, {i, j, ...}]` replaces the abstract indexes `idx[1]`, `idx[2]`, ... in *expr* by the index names *i*, *j*, ....

## Details
- `sumNameIndex` is the inverse of `sumAbstractIndex`: `sumNameIndex[sumAbstractIndex[expr, idx], idx, names]` gives sums with common dummy names, so that equivalent sums are combined.
- The list of names must contain at least as many entries as the largest number of indexes of a sum; abstract indexes without a name are left unchanged.
- The names should be declared with `snegfreeindexes` (or as lattice or momentum indexes).

## Examples
### Basic Examples
```wl
snegfreeindexes[i, j, k, l];
snegrealfunctions[f, g];
s = sum[f[i] g[j], {i, j}] + sum[f[k] g[l], {k, l}]
```
```wl
sumNameIndex[sumAbstractIndex[s, idx], idx, {i, j}]
```

### Properties & Relations
Abstract indexes without a name are kept:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f];
sumNameIndex[sumAbstractIndex[sum[f[i, j], {i, j}], idx], idx, {i}]
```
