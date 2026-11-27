---
keywords: simplify symbolic sums, FullSimplify, Kronecker delta, momentum shift, commutator of sums
see also: sum, sumSimplify, sumExpand, sumCollect, sumSimplifyKD, sumMomentumCanonical, SimplifyKD, FullSimplify
---
# sumFullSimplify

## Usage
- `sumFullSimplify[expr]` simplifies an expression with symbolic sums by `FullSimplify` with additional transformation functions for sums.

## Details
- The transformation functions passed to `FullSimplify` are:
- a fixed sequence of `Expand`, `sumCollect`, `Simplify`, `sumSimplifyKD`, `SimplifyKD`, `sumExpand`, and `sumSimplify` steps;
- combinations of expanding, collecting, and simplifying used by `sumSimplify`, `SimplifyKD`, and the individual rules of `sumExpand`, `sumCollect`, and `sumSimplifyKD`;
- `sumCollect[sumMomentumCanonical[sumExpand[#]]] &`, which shifts lattice and momentum summation indexes to a canonical form and renames automatically renamed dummy indexes (`k1`, ...) back to their root names, so that equivalent sums cancel.
- `FullSimplify` keeps the form with the smallest complexity (by default, essentially the leaf count). The result can therefore be collected or expanded, and is not a canonical form. Two expressions are equal if `sumFullSimplify` of their difference gives 0; a nonzero result does not prove that they differ.
- The momentum shifts are exact only for declared lattice and momentum indexes, see `sumMomentumCanonical`. Sums over generic indexes with different dummy names are not identified.
- `sumFullSimplify` can be slow for large expressions. `sumSimplify` uses `Simplify` with a subset of the transformations; for predictable results, apply `sumExpand`, `sumSimplifyKD`, and `sumCollect` explicitly.

## Examples
### Basic Examples
Commutator of the particle number with the band Hamiltonian, $[N, H] = 0$:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegrealfunctions[eps];
H = sum[eps[k] number[c[k]], {k}];
c1 = komutator[sum[number[c[k]], {k}], H];
sumFullSimplify[c1]
```

### Scope
Sums over Kronecker deltas:
```wl
snegfermionoperators[c];
snegfreeindexes[k, p];
sumFullSimplify[sum[KroneckerDelta[k, p] nc[c[CR, k, UP], c[AN, p, UP]], {k, p}]]
```
---
Collecting sums:
```wl
snegfermionoperators[c];
snegfreeindexes[k, sigma];
sumFullSimplify[sum[number[c[k], sigma], {k, sigma}] - sum[1, {k}]]
```
---
Invariance of lattice sums under momentum shifts, $\sum_k n_{k+q} = \sum_k n_k$:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
sumFullSimplify[sum[number[ck[k + q]], {k}] - sum[number[ck[k]], {k}]]
```

### Applications
Density operators $\rho_q = \sum_p c^\dagger_{p+q\uparrow} c_{p\uparrow}$ on a lattice commute, $[\rho_q, \rho_{-q}] = 0$:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{p, q}, Ns];
rho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], {p}];
sumFullSimplify[komutator[rho[q], rho[-q]]]
```

### Properties & Relations
`sumSimplify` does not shift momenta and does not rename dummy indexes:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{p, q}, Ns];
rho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], {p}];
sumSimplify[komutator[rho[q], rho[-q]]]
```
---
The same result is obtained with explicit steps:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegrealfunctions[eps];
c1 = komutator[sum[number[c[k]], {k}], sum[eps[k] number[c[k]], {k}]];
sumCollect[sumSimplifyKD[sumExpand[c1]]]
```

### Possible Issues
Equivalent sums over generic indexes with different dummy names are not identified:
```wl
snegfermionoperators[a];
snegfreeindexes[l, Q];
rho[qq_] := sum[nc[a[CR, l + qq, UP], a[AN, l, UP]], {l}];
sumFullSimplify[komutator[rho[Q], rho[-Q]]]
```
For a lattice, declare the indexes with `snegmomentumindexes`; for generic indexes, rename the dummy indexes with `sumAbstractIndex` and `sumNameIndex`:
```wl
sumNameIndex[sumAbstractIndex[%, idx], idx, {l}]
```
