---
keywords: shift summation variable, momentum shift, change of variables, symbolic sum, Umklapp, Tomonaga-Luttinger, anomalous commutator
see also: sumMomentumCanonical, sumFullSimplify, sum, snegmomentumindexes, sneglatticeindexes, ordering
---
# sumShift

## Usage
- `sumShift[expr, k -> k + q]` replaces the summation variable $k$ by $k + q$ in all sums over $k$ in *expr*.
- `sumShift[expr, k -> -k + q]` applies the reflection $k \to -k + q$.

## Details
- The change of variables $\sum_k f(k) = \sum_k f(\pm k + q)$ is exact if $k \to \pm k + q$ is a bijection of the index set. This holds for declared lattice and momentum indexes (`sneglatticeindexes`, `snegmomentumindexes`): momenta are defined modulo reciprocal-lattice vectors and sites modulo the lattice period.
- *k* must be a declared lattice or momentum index, the coefficient of *k* must be $\pm 1$, and the shift *q* must not contain *k* and must contain only quantities of the same type (momenta for a momentum index; lattice indexes and lattice vectors for a lattice index). Otherwise a message is issued and *expr* is returned unchanged.
- Sums whose summand contains fermion operators with `SEA` ordering are not shifted (message `sumShift::sea`). For a linearized band with unbounded momenta, the shift is not a symmetry of the Fermi sea; this is the origin of the anomalous (Schwinger) term in the commutator of density operators of the Tomonaga-Luttinger model.
- Only sums over *k* are changed; a free *k* outside sums is not replaced.
- `sumMomentumCanonical` chooses the shifts automatically.

| `sumShift::index` | *k* is not a declared lattice or momentum index |
| `sumShift::shift` | the substitution is not of the form $\pm k + q$ with *q* of the same index type |
| `sumShift::sea` | the sum contains `SEA`-ordered operators and is not shifted |

## Examples
### Basic Examples
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
sumShift[sum[number[ck[k + q]], {k}], k -> k - q]
```

### Scope
Reflection $k \to -k$:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
sumShift[sum[number[ck[k], UP], {k}], k -> -k]
```
---
Shift of a lattice index by a lattice vector:
```wl
snegfermionoperators[c];
sneglatticeindexes[{R}, Ns];
sneglatticevectors[a1];
sumShift[sum[hop[c[R], c[R + a1]], {R}], R -> R - a1]
```

### Applications
Two forms of the same density operator, $\rho_q = \sum_k c^\dagger_{k+q} c_k = \sum_k c^\dagger_k c_{k-q}$:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
rho1 = sum[nc[ck[CR, k + q, UP], ck[AN, k, UP]], {k}];
rho2 = sum[nc[ck[CR, k, UP], ck[AN, k - q, UP]], {k}];
sumShift[rho1, k -> k - q] - rho2
```

### Possible Issues
Generic indexes are not shifted, because the shift is not exact for a finite or open index set:
```wl
snegfreeindexes[n];
snegrealfunctions[f];
sumShift[sum[f[n], {n}], n -> n + 1]
```
---
Only shifts with coefficient $\pm 1$ are allowed:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k}, Ns];
sumShift[sum[number[ck[k]], {k}], k -> 2 k]
```
---
Sums with `SEA`-ordered operators are not shifted:
```wl
snegfermionoperators[a];
ordering[a] = SEA;
snegmomentumindexes[{k, q}, Ns];
sumShift[sum[number[a[k + q], UP], {k}], k -> k - q]
```

### Neat Examples
Anomalous commutator of the Tomonaga-Luttinger model. For right movers with a linearized dispersion, $\langle[\rho_q, \rho_{-q}]\rangle = \sum_k [\theta(-k) - \theta(q - k)]$. For $q > 0$ this is minus the number of momenta in the interval $0 < k \le q$, $-q L/(2\pi)$ for a system of length $L$; it would vanish if the shift $k \to k + q$ were allowed:
```wl
snegfermionoperators[a];
ordering[a] = SEA;
snegmomentumindexes[{k, q}, Ns];
rho[qq_] := sum[nc[a[CR, k + qq, UP], a[AN, k, UP]], {k}];
sumSimplifyKD[vev[komutator[rho[q], rho[-q]]]]
```
