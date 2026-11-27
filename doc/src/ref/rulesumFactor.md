---
keywords: factorize sum, separable sum, product of sums, symbolic sum
see also: FactorSeparableSumCustom, sumSimplifyKD, sumExpand, sum
---
# rulesumFactor

## Usage
- `expr /. rulesumFactor` factorizes the sums in *expr* into products of independent sums, $\sum_{ij} f_i g_j = \sum_i f_i \sum_j g_j$.

## Details
- `rulesumFactor` is `{s:sum[_, _List] :> FactorSeparableSumCustom[s]}`; see `FactorSeparableSumCustom` for the factorization.
- The summand must be a product of factors that depend on disjoint sets of indexes. A summand that is a sum of terms is not factorized; apply `sumExpand` first.

## Examples
### Basic Examples
```wl
snegfreeindexes[i, j];
snegrealfunctions[f, g];
sum[3 f[i] g[j], {i, j}] /. rulesumFactor
```

### Applications
The square of the total particle number in a Fermi sea, $\langle N \rangle^2 = \sum_{kl} n_k n_l$, from the separable double sum:
```wl
snegfreeindexes[k, l];
sum[UnitStep[-k] UnitStep[-l], {k, l}] /. rulesumFactor
```

### Possible Issues
A summand that is a sum of separable terms is not factorized:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f, g, h];
s = sum[f[i] g[j] + f[i] h[j], {i, j}];
s /. rulesumFactor
```
After `sumExpand`, each sum is factorized separately:
```wl
sumExpand[s] /. rulesumFactor
```
