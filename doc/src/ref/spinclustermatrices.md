---
keywords: exact diagonalization, spin cluster, Heisenberg model, spin Hamiltonian matrix, total Sz subspaces, Kronecker product, sparse matrix
see also: snegspinoperators, spinspin, stevens, spinketbrarules, spinmatrixX, makematricesbzvc, jordanwigner
---
# spinclustermatrices

## Usage
- `spinclustermatrices[H, {S[1], S[2], ...}]` gives the matrices of the spin Hamiltonian *H* in the subspaces of total $S^z$, as a list of pairs `{{Sz}, matrix}`.
- `spinclustermatrices[H, {S -> {j1, j2, ...}}]` specifies the sites by their indexes.
- `spinclustermatrices[H, sites, "Blocks" -> False]` gives the matrix in the full product space.

## Details
- *H* is a polynomial in spin operators `S[j..., t]` (components $t = 1, \ldots, 5$ for $S^x, S^y, S^z, S^+, S^-$) of the listed sites, with scalar coefficients. All spins must have a declared numeric size (`snegspinoperators[{S, s}]`); different spin symbols and sizes can be combined.
- The basis is the product basis $|m_1, m_2, \ldots, m_N\rangle$ with the sites in the given order, each $m_k$ running from $+s_k$ down to $-s_k$, and the first site varying slowest. The matrices are built from Kronecker products of the spin matrices `spinmatrixX[s]`, ..., so the computation does not use the operator algebra and is fast.
- With `"Blocks" -> True` (default), the matrix is split into the subspaces of total $S^z = \sum_k m_k$, in increasing order of $S^z$; within a subspace, the states are in the order of the product basis.
- The site specification is a list of elements `S[j...]` or rules `S -> {j1, j2, ...}`. Since the last index of a spin operator is the component, `S[0]` evaluates to 0; site 0 must therefore be given as `S -> {0, ...}`. Sites with several indexes are given as `S -> {{1, "a"}, {1, "b"}}`.
- The following options can be given:

| `"Blocks"` | `True` | split the matrix into the subspaces of total $S^z$ |
| `"Sparse"` | `False` | return `SparseArray` objects instead of dense matrices |

- Failure behavior: an operator that does not belong to the listed sites (or a non-spin operator) gives `spinclustermatrices::op`; an operator that does not conserve the total $S^z$ with `"Blocks" -> True` gives `spinclustermatrices::sz`; a spin without a declared numeric size or an invalid site specification gives a message from the site parser. In all cases `$Failed` is returned.
- For spins coupled to fermions, use `spinketbrarules` and `QSZaddspinket` with `makematricesbzvc`.

## Examples
### Basic Examples
Heisenberg dimer of two spins 1/2: triplet $J/4$ and singlet $-3J/4$:
```wl
snegspinoperators[{S, 1/2}];
snegrealconstants[J];
spinclustermatrices[J spinspin[S[1], S[2]], {S[1], S[2]}]
```
The full matrix in the basis $|{\uparrow\uparrow}\rangle, |{\uparrow\downarrow}\rangle, |{\downarrow\uparrow}\rangle, |{\downarrow\downarrow}\rangle$:
```wl
spinclustermatrices[J spinspin[S[1], S[2]], {S[1], S[2]}, "Blocks" -> False] // MatrixForm
```

### Scope
Mixed spins 1/2 and 1:
```wl
snegspinoperators[{S, 1/2}, {T, 1}];
spinclustermatrices[spinspin[S[1], T[1]], {S[1], T[1]}]
```
---
Sites given as a rule, including site 0:
```wl
snegspinoperators[{S, 1/2}];
spinclustermatrices[Sum[nc[S[0, a], S[1, a]], {a, 3}], {S -> {0, 1}}]
```
---
Sites with several indexes:
```wl
snegspinoperators[{S, 1/2}];
spinclustermatrices[S[1, "a", 3] + S[1, "b", 3], {S -> {{1, "a"}, {1, "b"}}}]
```
---
Single-ion anisotropy and Zeeman term of a spin 1:
```wl
snegspinoperators[{T, 1}];
snegrealconstants[Dz, B];
spinclustermatrices[Dz nc[T[1, 3], T[1, 3]] + B T[1, 3], {T[1]}]
```

### Options
#### Blocks
Operators that do not conserve $S^z_\mathrm{tot}$, such as a transverse field, require `"Blocks" -> False`:
```wl
snegspinoperators[{S, 1/2}];
spinclustermatrices[S[1, 1], {S[1]}, "Blocks" -> False]
```
#### Sparse
Sparse matrices for larger clusters:
```wl
snegspinoperators[{S, 1/2}];
m = spinclustermatrices[Sum[spinspin[S[i], S[i + 1]], {i, 9}], {S -> Range[10]}, "Sparse" -> True];
{Length[m], Head[m[[6, 2]]], Dimensions[m[[6, 2]]]}
```

### Applications
Antiferromagnetic Heisenberg ring of four spins 1/2. The lowest energy in each $S^z$ subspace shows the singlet ground state at $-2J$:
```wl
snegspinoperators[{S, 1/2}];
snegrealconstants[J];
H = J Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}];
{First[#], Min[Eigenvalues[Last[#]] /. J -> 1]} & /@ spinclustermatrices[H, {S -> Range[4]}]
```
---
Ground-state energy per site of the open Heisenberg chain of 12 spins 1/2 (with sparse matrices). The value approaches the Bethe-ansatz result $1/4 - \ln 2 \approx -0.4431$ of the infinite chain:
```wl
snegspinoperators[{S, 1/2}];
n = 12;
m = spinclustermatrices[Sum[spinspin[S[i], S[i + 1]], {i, n - 1}], {S -> Range[n]}, "Sparse" -> True];
h0 = Last[First[Select[m, First[#] == {0} &]]];
{Dimensions[h0], First[Eigenvalues[N[h0], 1]]/n, 1/4 - Log[2.]}
```
---
Spin-1 Heisenberg dimer with single-ion anisotropy $D (S^z)^2$ on both sites:
```wl
snegspinoperators[{T, 1}];
snegrealconstants[J, Dz];
H = J spinspin[T[1], T[2]] + Dz (stevens[2, 0, T[1]] + stevens[2, 0, T[2]] + 4)/3;
{First[#], Eigenvalues[Last[#]]} & /@ spinclustermatrices[H /. {J -> 1, Dz -> 1/2}, {T[1], T[2]}]
```

### Properties & Relations
For a single spin, the matrices are the spin matrices `spinmatrixX`, ...:
```wl
snegspinoperators[{T, 1}];
Table[spinclustermatrices[T[1, t], {T[1]}, "Blocks" -> False], {t, {1, 4}}] == {spinmatrixX[1], spinmatrixP[1]}
```
---
The Jordan-Wigner transformation gives the same spectrum in the fermionic representation:
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
H = Sum[spinspin[S[i], S[i + 1]], {i, 3}];
e1 = Sort[Flatten[Eigenvalues /@ spinclustermatrices[H, {S -> Range[4]}][[All, 2]]]];
e2 = Sort[Flatten[Eigenvalues /@ makematricesbzop[jordanwigner[H, S -> f, Range[4]], qszbasis[Table[f[i], {i, 4}]]][[All, 2]]]];
Chop[N[e1 - e2]]
```

### Possible Issues
The total $S^z$ must be conserved for the block structure:
```wl
snegspinoperators[{S, 1/2}];
spinclustermatrices[S[1, 1], {S[1]}]
```
---
All operators must belong to the listed sites:
```wl
snegspinoperators[{S, 1/2}];
spinclustermatrices[spinspin[S[1], S[2]], {S[1]}]
```
---
A declared numeric spin size is required:
```wl
snegspinoperators[G];
spinclustermatrices[G[1, 3], {G[1]}]
```
