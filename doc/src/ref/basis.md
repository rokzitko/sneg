---
keywords: single-site basis, local Fock space, creation operator representation, many-particle states
see also: makebasis, direct, nonebasis, qszbasis, matrixrepresentationop
---
# basis

## Usage
- `basis[c[i]]` gives the basis of the Fock space of the level `c[i]` as a list of creation operator strings.

## Details
- The states are represented by operator strings acting on the vacuum: 1 (empty level), $c^\dagger_{i\uparrow}$, $c^\dagger_{i\downarrow}$, and $c^\dagger_{i\downarrow} c^\dagger_{i\uparrow}$ for spin-1/2 fermions (the last string in normal order, $c^\dagger_{\downarrow} c^\dagger_{\uparrow} |0\rangle = -c^\dagger_{\uparrow} c^\dagger_{\downarrow} |0\rangle$).
- For fermions with spin *s* the list contains all $2^{2s+1}$ states, ordered by the number of particles.
- The level is given without the creation/annihilation and spin indexes. `basis` is `Listable`.
- `basis` does not define `BASIS` and does not depend on it. Bases of several levels are obtained with `direct`, or with the generators `nonebasis`, `qbasis`, `qszbasis`, ... for symmetry-adapted bases.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
basis[d[]]
```

### Scope
Several levels:
```wl
snegfermionoperators[c];
basis[{c[1], c[2]}]
```
---
A spin-1 fermion level has $2^3$ states:
```wl
snegfermionoperators[{f, 1}];
basis[f[]]
```

### Applications
Matrix of the Hubbard atom Hamiltonian in the basis of creation operator strings:
```wl
snegfermionoperators[d];
snegrealconstants[eps, U];
matrixrepresentationop[eps number[d[]] + U hubbard[d[]], basis[d[]]] // MatrixForm
```
---
Fock space of two sites as the direct product of single-site bases:
```wl
snegfermionoperators[c];
b2 = direct[basis[c[1]], basis[c[2]]];
Length[b2]
```

### Properties & Relations
The states are orthonormal:
```wl
snegfermionoperators[d];
b = basis[d[]];
Outer[scalarproductop, b, b]
```
