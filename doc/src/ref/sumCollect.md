---
keywords: collect sums, combine sums, symbolic sum, common indexes
see also: sum, sumExpand, sumSimplifyKD, sumFullSimplify, sumMomentumCanonical, sumAbstractIndex
---
# sumCollect

## Usage
- `sumCollect[expr]` combines symbolic sums over common indexes into single sums, $\sum_k a_k + \sum_k b_k \to \sum_k (a_k + b_k)$.

## Details
- `sumCollect` applies two rules repeatedly to pairs of terms $z_1 \sum a_1 + z_2 \sum a_2$ whose prefactors $z_{1,2}$ do not depend on the summation indexes:
- If both sums run over the same indexes, they are combined into $\sum (z_1 a_1 + z_2 a_2)$.
- If the index lists overlap partially, the common indexes form the outer sum: $\sum_{ij} a_{ij} + \sum_i b_i \to \sum_i \bigl(\sum_j a_{ij} + b_i\bigr)$.
- Sums with different names of the dummy indexes are not combined, because `sumCollect` does not rename indexes. Use `sumMomentumCanonical` (lattice and momentum indexes) or `sumAbstractIndex` and `sumNameIndex` to bring the dummy indexes to common names first.
- Terms without sums are left outside the sums.
- `sumExpand` is the inverse operation. `sumFullSimplify` combines `sumCollect` with other simplification steps.

## Examples
### Basic Examples
```wl
snegfreeindexes[k];
snegrealfunctions[f, g];
snegrealconstants[x];
sumCollect[2 sum[f[k], {k}] - x sum[g[k], {k}]]
```

### Scope
Partially overlapping index lists:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f, g];
sumCollect[sum[f[i, j], {i, j}] + sum[g[i], {i}]]
```
---
Terms without sums stay outside:
```wl
snegfreeindexes[k];
snegrealfunctions[f];
sumCollect[sum[f[k], {k}] + 3]
```
---
The number of electrons in a band measured from one electron per $k$ point, $\sum_{k\sigma} n_{k\sigma} - \sum_k 1$:
```wl
snegfermionoperators[c];
snegfreeindexes[k, sigma];
sumCollect[sum[number[c[k], sigma], {k, sigma}] - sum[1, {k}]]
```

### Applications
Two sums that differ only by the shift of a momentum cancel, $\sum_k n_{k+q} - \sum_k n_k = 0$, on a lattice:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
dn = sum[number[ck[k + q], UP], {k}] - sum[number[ck[k], UP], {k}];
sumCollect[dn]
```
The shift $k \to k - q$ is applied by `sumMomentumCanonical`:
```wl
sumCollect[sumMomentumCanonical[dn]]
```

### Properties & Relations
`sumCollect` is the inverse of `sumExpand`:
```wl
snegfreeindexes[k];
snegrealfunctions[f, g];
sumExpand[sum[f[k] + g[k], {k}]]
```
```wl
sumCollect[%]
```

### Possible Issues
Sums with different dummy index names are not combined:
```wl
snegfreeindexes[k, l];
snegrealfunctions[f, g];
sumCollect[sum[f[k], {k}] + sum[g[l], {l}]]
```
Rename the dummy index first:
```wl
sumCollect[sum[f[k], {k}] + (sum[g[l], {l}] /. l -> k)]
```
