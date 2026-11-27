---
keywords: basis conversion, creation operator representation, occupation number representation, symmetry-adapted basis
see also: bzvc2bzop, ops2vc, vc2ops, vacuum, makebasis, qsbasis, qszbasis, makematricesbzvc
---
# bzop2bzvc

## Usage
- `bzop2bzvc[bz]` converts the basis *bz* from the creation operator representation to the occupation number representation.
- `bzop2bzvc[bz, vac]` applies the operator expressions of the states to the vector *vac*.

## Details
- *bz* is a basis `{{qn, states}, ...}` (see `makematricesbzvc`). Each state $f$ is converted to $f |\mathrm{vac}\rangle$ with `ap[f, vac]`. The default *vac* is `vacuum[]`, the empty state `vc[0, 0, ...]` of the current `BASIS`.
- The basis generators `qszbasis`, `qsbasis`, ... set `BASIS` with `makebasis`. For bases constructed otherwise, call `makebasis` with all levels that occur before the conversion.
- `bzvc2bzop` is the inverse conversion. For larger systems, convert operator-form bases with `bzop2bzvc` and compute matrices with `makematricesbzvc`, which is much faster than `makematricesbzop`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
bz = qsbasis[{c[1], c[2]}]
```
```wl
bzop2bzvc[bz]
```

### Scope
A basis with phonon kets; the kets are appended to the occupation number vectors:
```wl
snegfermionoperators[d];
bzop2bzvc[transformtoPH[qsbasis[{d[]}], {1}]]
```

### Properties & Relations
The result agrees with the basis generated directly in the occupation number representation:
```wl
snegfermionoperators[c];
Expand[bzop2bzvc[qsbasis[{c[1], c[2]}]]] === Expand[qsbasisvc[{c[1], c[2]}]]
```
