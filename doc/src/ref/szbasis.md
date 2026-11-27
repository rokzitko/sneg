---
keywords: symmetry-adapted basis, spin projection, U(1) spin symmetry, creation operator representation
see also: szbasisvc, qszbasis, sbasis
---
# szbasis

## Usage
- `szbasis[{c[1], c[2], ...}]` gives the basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the creation operator representation, organized in subspaces with well-defined spin projection $S_z$.

## Details
- `szbasis[ops]` is `bzvc2bzop[szbasisvc[ops]]`; see `szbasisvc` and `qszbasisvc`.
- NRG Ljubljana uses `szbasis` for the symmetry types `SPU1` and `SPU1LR`.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
szbasis[{d[]}]
```

### Properties & Relations
```wl
snegfermionoperators[c];
bzop2bzvc[szbasis[{c[1], c[2]}]] === szbasisvc[{c[1], c[2]}]
```
