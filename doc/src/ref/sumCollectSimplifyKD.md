---
keywords: Kronecker delta, collect, symbolic sum
see also: sumSimplifyKD, sumExpand, sum, Collect
---
# sumCollectSimplifyKD

## Usage
- `sumCollectSimplifyKD[expr]` collects the summand of every sum in *expr* with respect to Kronecker deltas and then applies `sumSimplifyKD`.

## Details
- The summands are rewritten with `Collect[a, KroneckerDelta[__]]`, so that all terms with the same delta are combined into a single coefficient, $\delta_{ij} a_i + \delta_{ij} b_i \to \delta_{ij}(a_i + b_i)$, and products such as $\delta_{ij} (a_i + b_i) c_i$ are written as a delta times a coefficient. The deltas are then eliminated by `sumSimplifyKD`.
- Compared with `sumSimplifyKD[sumExpand[expr]]`, the coefficients of the deltas are not expanded.

## Examples
### Basic Examples
```wl
snegfreeindexes[i, j];
snegrealfunctions[f, g, h];
sumCollectSimplifyKD[sum[(f[i] + g[i]) h[i] KroneckerDelta[i, j], {i}]]
```
The summand of the sum with a delta and a remainder is collected before the delta is eliminated:
```wl
sumCollectSimplifyKD[sum[(1 + KroneckerDelta[i, j]) f[i], {i}]]
```

### Properties & Relations
With `sumExpand`, the coefficients are expanded:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f, g, h];
s = sum[(f[i] + g[i]) h[i] (1 + KroneckerDelta[i, j]), {i}];
{sumCollectSimplifyKD[s], sumSimplifyKD[sumExpand[s]]}
```
