---
keywords: Fock space, basis without symmetries, creation operator representation
see also: nonebasisvc, qbasis, basis, direct
---
# nonebasis

## Usage
- `nonebasis[{c[1], c[2], ...}]` gives the basis of the full Fock space of the levels `c[1]`, `c[2]`, ... in the creation operator representation, as a single subspace without quantum numbers.

## Details
- `nonebasis[ops]` is `bzvc2bzop[nonebasisvc[ops]]`. The result is `{{{}, {states}}}`. `nonebasis` calls `makebasis` with its argument.
- NRG Ljubljana uses `nonebasis` for the symmetry type `NONE`.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
nonebasis[{d[]}]
```

### Properties & Relations
```wl
snegfermionoperators[c];
bzop2bzvc[nonebasis[{c[1], c[2]}]] === nonebasisvc[{c[1], c[2]}]
```
