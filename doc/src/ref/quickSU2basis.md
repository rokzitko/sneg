---
keywords: isospin, charge SU(2), symmetry-adapted basis, particle-hole symmetry, NRG
see also: quickISObasis, quickISOSZbasis, transformQStoIS, qbasisvc, isospin, nnop, quickDBL
---
# quickSU2basis

## Usage
- `quickSU2basis[{c[1], c[2], ...}]` gives a basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the creation operator representation, organized in subspaces with well-defined total isospin $I$; each subspace contains the states with $I_z = I$.

## Details
- `quickSU2basis` applies `transformQStoIS` to the charge basis of `qbasisvc`. The result is `{{{I}, {states}}, ...}`; each state represents an isospin multiplet of $2I+1$ states.
- The isospin phases are taken from `nnop`, which must be defined for all levels; see `quickISObasis` for the definitions, the conventions, and the pitfalls.
- The basis is appropriate for particle-hole symmetric Hamiltonians without spin symmetry, e.g. with spin-orbit coupling or a magnetic field in an arbitrary direction (the `SU2` symmetry type of NRG Ljubljana).

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
nnop[d[]] = 0;
quickSU2basis[{d[]}]
```

### Applications
Hubbard atom $H = \frac{U}{2} (n - 1)^2 - B S_x$ in a transverse field:
```wl
snegfermionoperators[d];
snegrealconstants[U, B];
nnop[d[]] = 0;
bz = quickSU2basis[{d[]}];
makebasis[{d[]}];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[U/2 isozsq[d[]] - B spinx[d[]], bzop2bzvc[bz]]
```

### Properties & Relations
Dimensions for two levels, and the total number of states:
```wl
snegfermionoperators[c];
nnop[c[1]] = 0; nnop[c[2]] = 1;
dims = {First[#], Length[Last[#]]} & /@ quickSU2basis[{c[1], c[2]}]
```
```wl
Total[(2 #[[1, 1]] + 1) #[[2]] & /@ dims]
```
