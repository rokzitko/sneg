---
keywords: symmetry-adapted basis, charge conservation, U(1) symmetry, particle number, invariant subspace, occupation number representation
see also: qbasis, qszbasisvc, qsbasisvc, nonebasisvc, spinlessbasis, makebasis, makematricesbzvc
---
# qbasisvc

## Usage
- `qbasisvc[{c[1], c[2], ...}]` gives the basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the occupation number representation, organized in subspaces with well-defined charge $Q$.

## Details
- The result is a list `{{{Q}, {states}}, ...}`; the states are `vc` vectors in lexicographic order. `qbasisvc` calls `makebasis` with its argument.
- $Q = N - N_\mathrm{s}/2$, where $N_\mathrm{s}$ is the number of single-particle states (the length of `BASIS`); for spin-1/2 fermions, $Q = N - N_\mathrm{levels}$. The subspace $Q$ has $\binom{N_\mathrm{s}}{N}$ states.
- `qbasisvc` works for fermions with any spin, including spinless fermions. It is the appropriate basis for Hamiltonians that conserve only the particle number, e.g. with spin-orbit coupling or with a magnetic field in a general direction.
- `qbasis` gives the same basis in the creation operator representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
qbasisvc[{d[]}]
```
```wl
InputForm[%]
```

### Scope
Spinless fermions on three sites:
```wl
snegspinlessfermionoperators[a];
{First[#], Length[Last[#]]} & /@ qbasisvc[{a[1], a[2], a[3]}]
```

### Applications
Hubbard atom in a transverse magnetic field, $H = U n_\uparrow n_\downarrow - B S_x$. $S_z$ is not conserved, but the charge is; the singly occupied states are split by $\pm B/2$:
```wl
snegfermionoperators[d];
snegrealconstants[U, B];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[U hubbard[d[]] - B spinx[d[]], qbasisvc[{d[]}]]
```

### Properties & Relations
The dimensions of the subspaces are binomial coefficients:
```wl
snegfermionoperators[c];
{Length /@ Last /@ qbasisvc[{c[1], c[2]}], Table[Binomial[4, n], {n, 0, 4}]}
```
