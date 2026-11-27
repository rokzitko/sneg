---
keywords: distribute sum, linearity, symbolic sum
see also: sumExpand, ruleExpandUnderSum, sumCollect, sum
---
# rulesumDistribute

## Usage
- `expr /. rulesumDistribute` distributes the symbolic sums in *expr* over the terms of their summands, $\sum_k (a_k + b_k) \to \sum_k a_k + \sum_k b_k$, without expanding the summands.

## Details
- `rulesumDistribute` is `{sum[a_ + b_, it_List] :> sum[a, it] + sum[b, it]}`. Use `//.` to distribute completely; `/.` splits off one term at each level.
- `sumExpand` also expands the summands with `ExpandAll`; `rulesumDistribute` acts only on summands that are already sums.

## Examples
### Basic Examples
```wl
snegfreeindexes[k];
snegrealfunctions[f, g, h];
s = sum[f[k] h[k] + g[k] (1 + h[k]), {k}];
s //. rulesumDistribute
```
`sumExpand` expands the products as well:
```wl
sumExpand[s]
```
---
A summand that is a product is not distributed:
```wl
snegfreeindexes[k];
snegrealfunctions[f, g, h];
sum[(f[k] + g[k]) h[k], {k}] //. rulesumDistribute
```
