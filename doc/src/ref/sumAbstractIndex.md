---
keywords: dummy index, abstract index, rename summation index, symbolic sum, collect equivalent sums
see also: sumNameIndex, sumCollect, sumMomentumCanonical, sumFullSimplify, sum
---
# sumAbstractIndex

## Usage
- `sumAbstractIndex[expr, idx]` replaces the summation indexes of every sum in *expr* by the abstract indexes `idx[1]`, `idx[2]`, ..., numbered in the order of the index list of each sum.

## Details
- Dummy indexes can be renamed freely. After `sumAbstractIndex`, sums that differ only in the names of their dummy indexes, such as $\sum_k f_k$ and $\sum_p f_p$, have identical forms and are combined by `Plus` automatically or by `sumCollect`.
- Each sum is renamed separately: the indexes of a sum over $\{i, j\}$ become `idx[1]` and `idx[2]` in the order of the sorted index list.
- `idx` is a symbol; it is given the attribute `NumericFunction`, so that `idx[n]` is treated as a scalar.
- `sumNameIndex[expr, idx, {i, j, ...}]` replaces `idx[1]`, `idx[2]`, ... by ordinary index names.
- The renaming does not change the summand otherwise. Sums whose summands differ by a permutation of the dummy indexes, such as $\sum_{ij} f_i g_j$ and $\sum_{ij} f_j g_i$, are not identified. Use this function for specific simplifications; the order of the indexes in each sum determines the result.

## Examples
### Basic Examples
```wl
snegfreeindexes[i, j];
snegrealfunctions[f];
sumAbstractIndex[sum[f[i, j], {i, j}], idx]
```

### Applications
Two sums with different dummy indexes, $\sum_k n_k - \sum_p n_p = 0$:
```wl
snegfermionoperators[c];
snegfreeindexes[k, p];
s = sum[number[c[k]], {k}] - sum[number[c[p]], {p}]
```
```wl
sumAbstractIndex[s, idx]
```
---
The commutator of density operators $\rho_Q = \sum_l c^\dagger_{l+Q} c_l$ with generic indexes gives, after the deltas are summed, two sums that differ only in the names of the dummy indexes, `l` and `l1`. They cancel after renaming:
```wl
snegfermionoperators[a];
snegfreeindexes[l, Q];
rho[qq_] := sum[nc[a[CR, l + qq, UP], a[AN, l, UP]], {l}];
r = sumSimplifyKD[sumExpand[komutator[rho[Q], rho[-Q]]]]
```
```wl
sumNameIndex[sumAbstractIndex[r, idx], idx, {l}]
```

### Possible Issues
Sums that differ by a permutation of the dummy indexes are not identified:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f, g];
sumAbstractIndex[sum[f[i] g[j], {i, j}] - sum[f[j] g[i], {i, j}], idx]
```
