---
keywords: Fock space, basis without symmetries, occupation number representation, exact diagonalization
see also: nonebasis, qbasisvc, qszbasisvc, makebasis, makematricesbzvc
---
# nonebasisvc

## Usage
- `nonebasisvc[{c[1], c[2], ...}]` gives the basis of the full Fock space of the levels `c[1]`, `c[2]`, ... in the occupation number representation, as a single subspace without quantum numbers.

## Details
- The result is `{{{}, {states}}}` with all $2^{N_\mathrm{s}}$ occupation number states in lexicographic order, where $N_\mathrm{s}$ is the length of `BASIS`. `nonebasisvc` calls `makebasis` with its argument.
- The basis is appropriate for Hamiltonians without any conserved quantities, and for checking symmetry-adapted calculations.
- `nonebasis` gives the same basis in the creation operator representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
nonebasisvc[{d[]}] // InputForm
```

### Applications
Full Hamiltonian matrix of a level with a pairing term and a transverse field, which conserve neither the charge nor $S_z$:
```wl
snegfermionoperators[d];
snegrealconstants[Delta, B];
H = Delta (nc[d[CR, UP], d[CR, DO]] + nc[d[AN, DO], d[AN, UP]]) - B spinx[d[]];
m = makematricesbzvc[H, nonebasisvc[{d[]}]];
MatrixForm[m[[1, 2]]]
```
```wl
Eigenvalues[m[[1, 2]]]
```

### Properties & Relations
The dimension is $2^{N_\mathrm{s}}$:
```wl
snegfermionoperators[c];
Length[nonebasisvc[{c[1], c[2], c[3]}][[1, 2]]]
```
