---
keywords: symmetry-adapted basis, total spin, SU(2) symmetry, charge conservation, Clebsch-Gordan coefficients, creation operator representation
see also: qsbasisvc, qszbasis, sbasis, quickISObasis, SU2basistensorproduct, transformtoLR, makematricesbzop, bzop2bzvc
related links: [H. R. Krishna-murthy, J. W. Wilkins, K. G. Wilson, Phys. Rev. B 21, 1003 (1980)](https://doi.org/10.1103/PhysRevB.21.1003)
---
# qsbasis

## Usage
- `qsbasis[{c[1], c[2], ...}]` gives a basis of the Fock space of the spin-1/2 levels `c[1]`, `c[2]`, ... in the creation operator representation, organized in subspaces with well-defined charge $Q$ and total spin $S$; each subspace contains the states with $S_z = S$.

## Details
- The states are strings of creation operators acting on the vacuum (1 is the vacuum). The construction, the quantum numbers $Q = N - N_\mathrm{levels}$ and $S$, and the phase conventions are the same as for `qsbasisvc`; `bzop2bzvc[qsbasis[ops]]` equals `qsbasisvc[ops]`.
- `qsbasis` calls `makebasis` with its argument. The operators must be spin-1/2 fermion operators; otherwise the message `qsbasis::spin` is issued and `$Failed` is returned.
- `qsbasis[{}]` is `{{{0, 0}, {1}}}`.
- The construction in the creation operator representation uses `spindown`, which normalizes states with `vev`; it is slower than `qsbasisvc` for more than a few levels. `transformtoLR` (mirror parity) and `SU2basistensorproduct` expect bases in this representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
qsbasis[{d[]}]
```
Two levels:
```wl
snegfermionoperators[c];
qsbasis[{c[1], c[2]}]
```

### Applications
Matrices of the Hubbard dimer Hamiltonian in the creation operator representation:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]];
makematricesbzop[H, qsbasis[{c[1], c[2]}]]
```

### Properties & Relations
Conversion to the occupation number representation:
```wl
snegfermionoperators[c];
Expand[bzop2bzvc[qsbasis[{c[1], c[2]}]]] === Expand[qsbasisvc[{c[1], c[2]}]]
```

### Possible Issues
Spinless fermions are not supported:
```wl
snegspinlessfermionoperators[a];
qsbasis[{a[1]}]
```
