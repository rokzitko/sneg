---
keywords: symmetry-adapted basis, charge conservation, spin projection, U(1) symmetry, invariant subspace, exact diagonalization, Fock space, Hubbard model, Anderson impurity model
see also: qszbasis, qsbasisvc, qbasisvc, szbasisvc, nonebasisvc, makebasis, makematricesbzvc, mergebasis, vc, bzvc2bzop
---
# qszbasisvc

## Usage
- `qszbasisvc[{c[1], c[2], ...}]` gives the basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the occupation number representation, organized in subspaces with well-defined charge $Q$ and spin projection $S_z$.
- `qszbasisvc[{c[1], ...}, 1/2]` uses a specialized implementation for spin-1/2 fermions.

## Details
- The result is a list `{{{Q, Sz}, {states}}, ...}` of invariant subspaces; the states are `vc` vectors. This is the standard form of a basis in SNEG (see `makematricesbzvc`, `bzQ`).
- `qszbasisvc` calls `makebasis` with its argument; the `vc` vectors refer to this `BASIS`. The levels are given without the creation/annihilation and spin indexes.
- The charge is measured from half filling: $Q = N - N_\mathrm{s}/2$, where $N$ is the number of particles and $N_\mathrm{s}$ the number of single-particle states (the length of `BASIS`). For spin-1/2 fermions, $N_\mathrm{s}/2$ is the number of levels, so that $Q = N - N_\mathrm{levels}$; the empty system has $Q = -N_\mathrm{levels}$, and the half-filled system $Q = 0$.
- The spin projection is $S_z = \sum_k m_k n_k$, where $m_k = \pm 1/2$ for the states `UP` and `DO` of spin-1/2 fermions; for fermions with spin *s*, $m_k$ is the spin index of the k-th element of `BASIS`.
- The basis states are the occupation number states `vc[n1, n2, ...]`, which are eigenstates of $Q$ and $S_z$; see `vc` for the sign convention. The subspaces are sorted by $(Q, S_z)$; within a subspace, the states are in lexicographic order of the occupation numbers. The specialized implementation `qszbasisvc[ops, 1/2]` gives the same subspaces with a different order of the states.
- The total number of states is $2^{N_\mathrm{s}}$, i.e. $4^{N_\mathrm{levels}}$ for spin-1/2 fermions.
- The $(Q, S_z)$ basis is appropriate for Hamiltonians that conserve the particle number and $S_z$, e.g. in a magnetic field along $z$ or with spin anisotropy along $z$. For SU(2) spin-symmetric Hamiltonians, `qsbasisvc` gives smaller subspaces.
- `qszbasis` gives the same basis in the creation operator representation.

## Developer Notes
- The generic implementation enumerates all $2^{N_\mathrm{s}}$ binary vectors, computes $Q$ and $S_z$ as scalar products with masks, and groups the states with `mergebasis`. The cost grows as $2^{N_\mathrm{s}}$; up to about 8 spin-1/2 levels are practical.
- NRG Ljubljana uses `qszbasis` for the symmetry types `QSZ`, `QSZLR`, `ISOSZ`, and `ISOSZLR`, and stores $S_z$ as $2S_z + 1$.

## Examples
### Basic Examples
The basis of a single spin-1/2 level:
```wl
snegfermionoperators[d];
qszbasisvc[{d[]}]
```
```wl
InputForm[%]
```

### Scope
Dimensions of the subspaces for two levels:
```wl
snegfermionoperators[c];
{First[#], Length[Last[#]]} & /@ qszbasisvc[{c[1], c[2]}]
```
The total dimension is $4^2$:
```wl
Total[Length /@ Last /@ qszbasisvc[{c[1], c[2]}]]
```
---
Levels of different operators:
```wl
snegfermionoperators[d, c];
qszbasisvc[{d[], c[1]}] // Length
```
---
Fermions with spin 1 (three components per level). The charge is measured from half filling of the three states:
```wl
snegfermionoperators[{f, 1}];
qszbasisvc[{f[]}] // InputForm
```
---
Spinless fermions: $S_z = 0$, and $Q$ is half-integer for an odd number of levels:
```wl
snegspinlessfermionoperators[a];
qszbasisvc[{a[1]}] // InputForm
```
---
The specialized spin-1/2 implementation gives the same subspaces:
```wl
snegfermionoperators[c];
Map[Sort, qszbasisvc[{c[1], c[2]}, 1/2], {2}] === Map[Sort, qszbasisvc[{c[1], c[2]}], {2}]
```

### Applications
Spectrum of the Hubbard atom $H = \epsilon\, n + U n_\uparrow n_\downarrow$ by subspace:
```wl
snegfermionoperators[d];
snegrealconstants[eps, U];
makematricesbzvc[eps number[d[]] + U hubbard[d[]], qszbasisvc[{d[]}]]
```
---
Hubbard dimer in a magnetic field, $H = -t \sum_\sigma (c^\dagger_{1\sigma} c_{2\sigma} + \mathrm{h.c.}) + U \sum_i n_{i\uparrow} n_{i\downarrow} - B (S^z_1 + S^z_2)$. The field splits the triplet; in the half-filled subspaces with $S_z = \pm 1$ the energies are $\mp B$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U, B];
H = -t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]] - B (spinz[c[1]] + spinz[c[2]]);
mats = makematricesbzvc[H, qszbasisvc[{c[1], c[2]}]];
{First[#], Eigenvalues[Last[#]]} & /@ Select[mats, First[First[#]] == 0 &]
```
---
Anderson impurity $d$ coupled to one bath level $c$, $H = \epsilon_d n_d + U n_{d\uparrow} n_{d\downarrow} + V \sum_\sigma (d^\dagger_\sigma c_\sigma + \mathrm{h.c.})$. Matrix in the subspace with one electron and $S_z = 1/2$:
```wl
snegfermionoperators[d, c];
snegrealconstants[epsd, U, V];
H = epsd number[d[]] + U hubbard[d[]] + V hop[d[], c[]];
bz = qszbasisvc[{d[], c[]}];
First[Select[makematricesbzvc[H, bz], First[#] == {-1, 1/2} &]]
```

### Properties & Relations
`qszbasis` gives the creation operator representation:
```wl
snegfermionoperators[d];
qszbasis[{d[]}]
```
```wl
bzvc2bzop[qszbasisvc[{d[]}]] === qszbasis[{d[]}]
```
---
The basis states are eigenstates of the charge and of the spin projection:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
Q = number[c[1]] + number[c[2]] - 2;
Sz = spinz[c[1]] + spinz[c[2]];
And @@ Flatten[Table[{Expand[ap[Q, v] - sub[[1, 1]] v] === 0, Expand[ap[Sz, v] - sub[[1, 2]] v] === 0}, {sub, bz}, {v, sub[[2]]}]]
```
---
Merging the $S_z$ subspaces gives the basis with only the charge quantum number, `qbasisvc`:
```wl
snegfermionoperators[c];
Map[Sort, mergebasis[{{First[#[[1]]]}, #[[2]]} & /@ qszbasisvc[{c[1], c[2]}]], {2}] === Map[Sort, qbasisvc[{c[1], c[2]}], {2}]
```

### Possible Issues
`qszbasisvc` redefines `BASIS`. States of an earlier basis must not be used afterwards:
```wl
snegfermionoperators[c, d];
bz1 = qszbasisvc[{c[1]}];
bz2 = qszbasisvc[{d[1]}];
{bz1[[2, 2, 1]], vc2ops[bz1[[2, 2, 1]]]}
```
