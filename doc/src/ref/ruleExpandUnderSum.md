---
keywords: expand summand, symbolic sum, Expand
see also: sumExpand, rulesumDistribute, ruleFullSimplifyUnderSum, sum, Expand
---
# ruleExpandUnderSum

## Usage
- `expr /. ruleExpandUnderSum` expands the summands of the symbolic sums in *expr* with `Expand`.

## Details
- `ruleExpandUnderSum` is `{sum[a_, it_List] :> sum[Expand[a], it]}`. The sums are not distributed over the resulting terms; combine with `rulesumDistribute` for that.
- `Expand` does not expand denominators, in contrast to `ExpandAll`, which is used by `sumExpand`.

## Examples
### Basic Examples
```wl
snegfreeindexes[k];
snegrealfunctions[f, g, h];
s = sum[(f[k] + g[k])/(1 + h[k])^2, {k}];
s /. ruleExpandUnderSum
```
`sumExpand` also distributes the sum and expands the denominator:
```wl
sumExpand[s]
```
```wl
s /. ruleExpandUnderSum //. rulesumDistribute
```
