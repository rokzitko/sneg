---
keywords: symmetry-adapted basis, charge conservation, spin projection, creation operator representation, invariant subspace
see also: qszbasisvc, qsbasis, qbasis, szbasis, nonebasis, makebasis, makematricesbzop, bzop2bzvc
---
# qszbasis

## Usage
- `qszbasis[{c[1], c[2], ...}]` gives the basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the creation operator representation, organized in subspaces with well-defined charge $Q$ and spin projection $S_z$.
- `qszbasis[{c[1], ...}, 1/2]` uses the specialized implementation for spin-1/2 fermions.

## Details
- `qszbasis[ops]` is `bzvc2bzop[qszbasisvc[ops]]`: the states are strings of creation operators (in the normal order of `nc`) that act on the vacuum; 1 is the vacuum. See `qszbasisvc` for the definition of the quantum numbers, $Q = N - N_\mathrm{s}/2$ and $S_z$, and for the order of the subspaces.
- `qszbasis` calls `makebasis` with its argument.
- Matrices of operators in this basis are obtained with `makematricesbzop`; `bzop2bzvc` converts the basis to the occupation number representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
qszbasis[{d[]}]
```

### Scope
Two levels:
```wl
snegfermionoperators[c];
qszbasis[{c[1], c[2]}]
```

### Applications
Spectrum of the Hubbard atom in a magnetic field, $H = \epsilon\, n + U n_\uparrow n_\downarrow - B S_z$:
```wl
snegfermionoperators[d];
snegrealconstants[eps, U, B];
makematricesbzop[eps number[d[]] + U hubbard[d[]] - B spinz[d[]], qszbasis[{d[]}]]
```

### Properties & Relations
Conversion to the occupation number representation:
```wl
snegfermionoperators[d];
bzop2bzvc[qszbasis[{d[]}]] === qszbasisvc[{d[]}]
```
