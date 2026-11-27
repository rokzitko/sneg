---
keywords: symmetry-adapted basis, total spin, SU(2) symmetry, creation operator representation
see also: sbasisvc, qsbasis, szbasis
---
# sbasis

## Usage
- `sbasis[{c[1], c[2], ...}]` gives a basis of the Fock space of the spin-1/2 levels `c[1]`, `c[2]`, ... in the creation operator representation, organized in subspaces with well-defined total spin $S$; each subspace contains the states with $S_z = S$.

## Details
- `sbasis[ops]` is `bzvc2bzop[sbasisvc[ops]]`; see `sbasisvc` and `qsbasisvc`. `sbasis[{}]` is `{{{0}, {1}}}`.
- NRG Ljubljana uses `sbasis` for the symmetry types `SPSU2` and `SPSU2LR` (spin symmetry without charge conservation).

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
sbasis[{d[]}]
```

### Properties & Relations
```wl
snegfermionoperators[c];
Expand[bzop2bzvc[sbasis[{c[1], c[2]}]]] === Expand[sbasisvc[{c[1], c[2]}]]
```
