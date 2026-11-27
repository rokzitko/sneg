---
keywords: isospin, charge SU(2), spin projection, symmetry-adapted basis, particle-hole symmetry, NRG
see also: quickISObasis, quickSU2basis, transformQStoIS, qszbasisvc, isospin, nnop, quickDBLSZ
---
# quickISOSZbasis

## Usage
- `quickISOSZbasis[{c[1], c[2], ...}]` gives a basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the creation operator representation, organized in subspaces with well-defined total isospin $I$ and spin projection $S_z$; each subspace contains the states with $I_z = I$.

## Details
- `quickISOSZbasis` applies `transformQStoIS` to the $(Q, S_z)$ basis of `qszbasisvc`. The result is `{{{I, Sz}, {states}}, ...}`; each state represents an isospin multiplet of $2I+1$ states.
- The isospin phases are taken from `nnop`, which must be defined for all levels; see `quickISObasis` for the definitions, the conventions, and the pitfalls.
- The basis is appropriate for particle-hole symmetric Hamiltonians that conserve $S_z$ but not the total spin, e.g. with a magnetic field along $z$ (the `ISOSZ` symmetry type of NRG Ljubljana).

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
nnop[d[]] = 0;
quickISOSZbasis[{d[]}]
```

### Applications
Hubbard atom $H = \frac{U}{2} (n - 1)^2 - B S_z$ in a magnetic field: the field splits the spin doublet, while the isospin doublet ($I = 1/2$, empty and doubly occupied states) remains degenerate:
```wl
snegfermionoperators[d];
snegrealconstants[U, B];
nnop[d[]] = 0;
bz = quickISOSZbasis[{d[]}];
makebasis[{d[]}];
makematricesbzvc[U/2 isozsq[d[]] - B spinz[d[]], bzop2bzvc[bz]]
```

### Properties & Relations
Dimensions for two levels, and the total number of states:
```wl
snegfermionoperators[c];
nnop[c[1]] = 0; nnop[c[2]] = 1;
dims = {First[#], Length[Last[#]]} & /@ quickISOSZbasis[{c[1], c[2]}]
```
```wl
Total[(2 #[[1, 1]] + 1) #[[2]] & /@ dims]
```
