---
keywords: spin-spin correlation, symmetrized exchange, average spin correlation
see also: spinspin, manyspin
---
# spinspinsymmetric

## Usage
- `spinspinsymmetric[{a, b, ...}]` gives the average $\frac{1}{N} \sum_{\text{pairs}} \mathbf S_a \cdot \mathbf S_b$ of the spin-spin products over all $N$ pairs of the fermion levels *a*, *b*, ....

## Details
- For $n$ levels, $N = n(n-1)/2$. The result is invariant under permutations of the levels.
- With the total spin $\mathbf S = \sum_a \mathbf S_a$, $\sum_{\text{pairs}} \mathbf S_a \cdot \mathbf S_b = \frac12 (\mathbf S^2 - \sum_a \mathbf S_a^2)$.
- Levels are given as `c[i]`; the result is simplified with `Simplify`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
Expand[spinspinsymmetric[{c[1], c[2], c[3]}] - (spinspin[c[1], c[2]] + spinspin[c[1], c[3]] + spinspin[c[2], c[3]])/3]
```

### Applications
Average spin correlation in the states of three electrons on three sites. With $\mathbf S^2 = S_t(S_t+1)$ and singly occupied sites, the average is $(S_t(S_t+1) - 9/4)/6$, i.e. $-1/4$ for $S_t = 1/2$ and $1/4$ for $S_t = 3/2$:
```wl
snegfermionoperators[c];
state = nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, UP], VACUUM];
Expand[nc[spinspinsymmetric[{c[1], c[2], c[3]}], state]]
```

### Properties & Relations
Relation with the total spin for singly occupied levels:
```wl
snegfermionoperators[c];
ops = {c[1], c[2], c[3]};
Expand[3 spinspinsymmetric[ops] - (manyspin[ops][[4]] - Total[spinss[ops]])/2]
```
