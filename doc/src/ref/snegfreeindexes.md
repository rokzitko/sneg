---
keywords: summation index, dummy index, free index, symbolic sum, momentum index, declaration
see also: sum, sumExpand, sumSimplifyKD, sumCollect, sneglatticeindexes, snegmomentumindexes, isnumericQ, listfreeindexes
---
# snegfreeindexes

## Usage
- `snegfreeindexes[k, q, ...]` declares *k*, *q*, ... as indexes that are used as summation variables in `sum[...]` and as operator indexes.

## Details
- Declared indexes are scalars (see `isnumericQ`): an index appearing in a coefficient, e.g. `k c[CR, k, UP]`, is factored out of products. They are not real: `conj[k]` is `Conjugate[k]`.
- The functions for symbolic sums (`sum`, `sumExpand`, `sumSimplifyKD`, `sumCollect`, ...) use the declaration to recognize indexes; when sums are multiplied, dummy indexes that occur in both factors are renamed (`k` to `k1`, `k2`, ...), and the new names are declared automatically.
- The declaration sets the upvalues `snegnonopQ[k] = True` and `freeindex[k] = True` and adds *k* to `listfreeindexes`.
- For lattice and momentum sums with periodic boundary conditions, use `sneglatticeindexes` and `snegmomentumindexes`, which also declare the indexes as summation indexes.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegfreeindexes[k];
nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, UP], {k}]]
```
Sums with Kronecker deltas are performed by `sumSimplifyKD`:
```wl
sumSimplifyKD[nc[c[AN, 1, UP], sum[c[CR, k, UP], {k}]]]
```

### Scope
Indexes are scalars, so they are factored out of products:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
nc[c[AN, 1, UP], k c[CR, 2, UP]]
```

### Properties & Relations
Indexes created by renaming are declared automatically:
```wl
snegfermionoperators[c];
snegfreeindexes[q];
nc[sum[c[CR, q, UP], {q}], sum[c[AN, q, UP], {q}]];
listfreeindexes
```

### Possible Issues
Indexes are not real; for a real index use `snegrealconstants` instead or in addition:
```wl
snegfreeindexes[k];
Conjugate[k]
```
