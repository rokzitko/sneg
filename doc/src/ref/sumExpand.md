---
keywords: expand sum, distribute sum, linearity, symbolic sum
see also: sum, sumCollect, sumSimplifyKD, sumFullSimplify, rulesumDistribute, ruleExpandUnderSum, ExpandAll
---
# sumExpand

## Usage
- `sumExpand[expr]` distributes the symbolic sums in *expr* over the terms of their summands, $\sum_k (a_k + b_k) \to \sum_k a_k + \sum_k b_k$, and expands the summands with `ExpandAll`.

## Details
- `sumExpand` applies the rules `sum[a + b, it] -> sum[a, it] + sum[b, it]` and `sum[a, it] -> sum[ExpandAll[a], it]` repeatedly, so nested sums are expanded as well. Factors free of the summation indexes are then moved out of the sums automatically.
- Expressions outside sums are not expanded; combine with `Expand` where needed.
- Most results of `komutator`, `nc`, and `vev` with sums must be expanded with `sumExpand` before `sumSimplifyKD` can eliminate Kronecker deltas term by term.
- `sumCollect` is the inverse operation.
- `ExpandAll` also expands denominators. To expand only the numerators, use `expr /. ruleExpandUnderSum`; to distribute without expanding, use `expr /. rulesumDistribute`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegrealfunctions[f];
s = sum[f[k] (number[c[k]] - 1), {k}]
```
```wl
sumExpand[s]
```

### Scope
Nested sums are expanded; the term that does not depend on `l` gives a factor $\sum_l 1$:
```wl
snegfreeindexes[k, l];
snegrealfunctions[g];
snegrealconstants[x];
sumExpand[sum[x + sum[g[k, l], {k}], {l}]]
```

### Applications
Equation of motion $[H, c^\dagger_{p\uparrow}] = \epsilon_p c^\dagger_{p\uparrow}$ for $H = \sum_k \epsilon_k n_{k\uparrow}$. The Kronecker delta is inside the product with $\epsilon_k$, and `sumSimplifyKD` can eliminate it only after expanding:
```wl
snegfermionoperators[c];
snegfreeindexes[k, p];
snegrealfunctions[eps];
r = komutator[sum[eps[k] number[c[k], UP], {k}], c[CR, p, UP]]
```
```wl
{sumSimplifyKD[r] === r, sumSimplifyKD[sumExpand[r]]}
```

### Properties & Relations
`sumCollect` combines sums with equal indexes:
```wl
snegfreeindexes[k];
snegrealfunctions[f, g, h];
sumExpand[sum[(f[k] + g[k]) h[k], {k}]]
```
```wl
sumCollect[%]
```

### Possible Issues
`ExpandAll` also expands denominators:
```wl
snegfreeindexes[k];
snegrealfunctions[f, g, h];
s = sum[(f[k] + g[k])/(1 + h[k])^2, {k}];
sumExpand[s]
```
`ruleExpandUnderSum` uses `Expand` and keeps the denominators:
```wl
s /. ruleExpandUnderSum
```
