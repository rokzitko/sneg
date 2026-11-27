---
keywords: thread sum over list, symbolic sum, list of operators
see also: sum, sumExpand, Thread
---
# sumThread

## Usage
- `sumThread[expr]` threads the sums with a list as summand over the list: `sum[{a, b, ...}, it]` becomes `{sum[a, it], sum[b, it], ...}`.

## Details
- `sum` does not thread over lists automatically; `sumThread` applies the rule `sum[{a, b, ...}, it] -> {sum[a, it], sum[b, it], ...}` repeatedly to *expr*.
- `Thread[sum[{a, b}, {k}]]` threads over all list arguments, including the index list, and fails for lists of different lengths; use `sumThread`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegfreeindexes[k];
sum[{number[c[k], UP], number[c[k], DO]}, {k}]
```
```wl
sumThread[%]
```

### Applications
The total spin of a band, $\mathbf S = \sum_k \mathbf s_k$, from the spin vector of each level given by `spinxyz`:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
sumThread[sum[spinxyz[c[k]], {k}]]
```

### Possible Issues
`Thread` also threads over the index list and fails:
```wl
snegfreeindexes[k];
snegrealfunctions[f, g];
Thread[sum[{f[k], g[k]}, {k}]]
```
```wl
sumThread[sum[{f[k], g[k]}, {k}]]
```
