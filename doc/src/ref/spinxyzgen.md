---
keywords: spin operator, generalized level, molecular orbital, bonding orbital, linear combination of operators
see also: spinxyz, spinspin, spinmatrixX
---
# spinxyzgen

## Usage
- `spinxyzgen[g]` gives the spin-1/2 operators $\{S^x, S^y, S^z\}$ of the generalized level with the creation operators `g[UP]` and `g[DO]`.
- `spinxyzgen[g, s]` gives the spin-*s* operators of the level with the creation operators `g[m]`, $m = s, s-1, \ldots, -s$.

## Details
- *g* is a function of the spin index that returns a creation operator or a linear combination of creation operators, e.g. `c[CR, 1, #] &` or `(c[CR, 1, #] + c[CR, 2, #])/Sqrt[2] &`. The annihilation operators are obtained with `conj`.
- With $g^\dagger_\sigma$ = `g[σ]`, the result is $S^a = \frac12 \sum_{\sigma\sigma'} g^\dagger_\sigma \sigma^a_{\sigma\sigma'} g_{\sigma'}$ in the basis $(\uparrow, \downarrow)$, and $S^a = \sum_{mm'} g^\dagger_m (S^a)_{mm'} g_{m'}$ with the spin-*s* matrices `spinmatrixX[s]`, ... for the second form.
- The operators are spin operators only if the $g_\sigma$ satisfy canonical anticommutation relations, e.g. for normalized orthogonal combinations of levels.
- The results are not expanded.
- `spinxyz[c[i]]` is `spinxyzgen[c[CR, i, #] &]`, and `spinxyz[f]` for a function of two arguments (operator type and spin) is `spinxyzgen[f[CR, #] &]`.

## Examples
### Basic Examples
Spin of the bonding orbital $b^\dagger_\sigma = (c^\dagger_{1\sigma} + c^\dagger_{2\sigma})/\sqrt2$:
```wl
snegfermionoperators[c];
Expand[spinxyzgen[(c[CR, 1, #] + c[CR, 2, #])/Sqrt[2] &][[3]]]
```
The state $b^\dagger_\uparrow|0\rangle$ has $S^z = 1/2$:
```wl
sz = spinxyzgen[(c[CR, 1, #] + c[CR, 2, #])/Sqrt[2] &][[3]];
Expand[nc[sz, c[CR, 1, UP] + c[CR, 2, UP], VACUUM]]
```

### Scope
Spin-1 operators of a three-component fermion:
```wl
snegfermionoperators[{f, 1}];
spinxyzgen[f[CR, #] &, 1]
```

### Properties & Relations
The conventional spin operators of a level:
```wl
snegfermionoperators[c];
spinxyzgen[c[CR, 1, #] &] == spinxyz[c[1]]
```
