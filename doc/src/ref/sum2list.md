---
keywords: list of terms, addends, Plus, split expression into terms
see also: sum, Expand, List
---
# sum2list

## Usage
- `sum2list[a + b + ...]` gives the list of terms `{a, b, ...}`.
- `sum2list[x]` gives `{x}` for an expression *x* that is not a sum.

## Details
- `sum2list` splits an expression with head `Plus` into its terms. It does not act on symbolic sums `sum[...]`: a symbolic sum is a single term.
- Apply `Expand` first to obtain the individual operator monomials of an expression.

## Examples
### Basic Examples
```wl
sum2list[a + b + c]
```
```wl
sum2list[x]
```

### Applications
The operator monomials of the hopping term:
```wl
snegfermionoperators[c];
sum2list[Expand[hop[c[1], c[2]]]]
```

### Possible Issues
A symbolic sum is not split:
```wl
snegfreeindexes[k];
snegrealfunctions[f, g];
sum2list[sum[f[k] + g[k], {k}]]
```
```wl
sum2list[sumExpand[sum[f[k] + g[k], {k}]]]
```
