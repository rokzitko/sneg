---
keywords: symmetry-adapted basis, charge conservation, U(1) symmetry, creation operator representation
see also: qbasisvc, qszbasis, qsbasis, nonebasis, spinlessbasis, quickDBL
---
# qbasis

## Usage
- `qbasis[{c[1], c[2], ...}]` gives the basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the creation operator representation, organized in subspaces with well-defined charge $Q$.

## Details
- `qbasis[ops]` is `bzvc2bzop[qbasisvc[ops]]`; see `qbasisvc` for the definition $Q = N - N_\mathrm{s}/2$. `qbasis` calls `makebasis` with its argument.
- NRG Ljubljana uses `qbasis` as the starting point for the symmetry types `U1`, `SL`, and `SU2` (isospin, after `transformQStoIS`), and for the charge-parity types `P` and `PP`, where $Q$ is replaced by $\pm 1$ according to the parity of $Q$ and the subspaces are merged with `mergebasis`.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
qbasis[{d[]}]
```

### Applications
Basis with the fermion parity $P = (-1)^N = (-1)^{Q + N_\mathrm{levels}}$ as the only quantum number, appropriate for superconducting systems, obtained by merging the charge subspaces of one level:
```wl
snegfermionoperators[d];
bz = mergebasis[{{(-1)^(#[[1, 1]] + 1)}, #[[2]]} & /@ qbasis[{d[]}]]
```
The BCS pairing term $\Delta (d^\dagger_\uparrow d^\dagger_\downarrow + \mathrm{h.c.})$ couples the empty and the doubly occupied state:
```wl
snegrealconstants[Delta];
makematricesbzop[Delta (nc[d[CR, UP], d[CR, DO]] + nc[d[AN, DO], d[AN, UP]]), bz]
```

### Properties & Relations
```wl
snegfermionoperators[c];
bzop2bzvc[qbasis[{c[1], c[2]}]] === qbasisvc[{c[1], c[2]}]
```
