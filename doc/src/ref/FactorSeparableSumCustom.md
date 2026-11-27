---
keywords: factorize sum, separable sum, product of sums, connected components
see also: rulesumFactor, sumSimplifyKD, sum
---
# FactorSeparableSumCustom

## Usage
- `FactorSeparableSumCustom[sum[expr, {i, j, ...}]]` factorizes a symbolic sum into a product of independent sums, $\sum_{ijk} f_i\, g_j\, h_{ik} = \bigl(\sum_{ik} f_i h_{ik}\bigr) \bigl(\sum_j g_j\bigr)$.

## Details
- The factors of the summand (the arguments of `Times`) and the summation indexes form a bipartite graph, in which a factor is connected with the indexes on which it depends. Each connected component gives one factor of the result: factors without indexes are moved out of the sum, indexes that do not appear in the summand give `sum[1, {i}]`, and all other components give a sum of the product of their factors over their indexes.
- The summand is not expanded; a summand that is a sum of terms is a single factor.
- `rulesumFactor` applies `FactorSeparableSumCustom` to all sums in an expression.

## Examples
### Basic Examples
```wl
snegfreeindexes[i, j, k];
snegrealfunctions[f, g, h];
FactorSeparableSumCustom[sum[f[i] g[j] h[i, k], {i, j, k}]]
```

### Scope
An index that does not appear in the summand gives a factor $\sum_k 1$:
```wl
snegfreeindexes[i, j, k];
snegrealfunctions[f, g];
FactorSeparableSumCustom[sum[f[i] g[j], {i, j, k}]]
```
---
Factors that connect all indexes are kept together:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f, g, h];
FactorSeparableSumCustom[sum[f[i] g[j] h[i, j], {i, j}]]
```
