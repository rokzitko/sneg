---
keywords: parity, mirror symmetry, reflection symmetry, even and odd channels, two-channel model, occupation number representation
see also: transformtoLR, lrmap, bzop2bzvc, vacuum, makematricesbzvc, qsbasis
---
# transformtoLRvc

## Usage
- `transformtoLRvc[bz, map, vac]` transforms the basis *bz* in the creation operator representation into a basis with the additional parity quantum number $P = \pm 1$, with the states in the occupation number representation obtained by acting on the vector *vac*.
- `transformtoLRvc[bz, map, vac, rules]` applies the additional substitution *rules* together with the reflection.

## Details
- The parity and the conventions are those of `transformtoLR`: the reflection is defined by `lrmap[map]`, and the parity is appended to the quantum numbers of each subspace.
- *bz* must be in the creation operator representation, e.g. from `qsbasis` or `qszbasis`. *vac* is normally `vacuum[]`, the empty occupation number vector of the current `BASIS`.
- The reflected states are formed by substitution in the operator expressions, converted with `bzop2bzvc`, and orthonormalized in the occupation number representation, which is much faster than `transformtoLR`.
- The result equals `bzop2bzvc[transformtoLR[bz, map, rules], vac]` and is used with `makematricesbzvc`.

## Examples
### Basic Examples
Parity-adapted basis of the Hubbard dimer in the occupation number representation:
```wl
snegfermionoperators[c];
lr = transformtoLRvc[qsbasis[{c[1], c[2]}], {c[1], c[2]}, vacuum[]]
```
```wl
snegrealconstants[t, U];
makematricesbzvc[-t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]], lr]
```

### Scope
Three sites with the middle site invariant under the reflection; dimensions of the $(Q, S, P)$ subspaces:
```wl
snegfermionoperators[c];
{First[#], Length[Last[#]]} & /@ transformtoLRvc[qsbasis[{c[1], c[2], c[3]}], {c[1], c[2], c[3]}, vacuum[]]
```

### Properties & Relations
The result agrees with `transformtoLR` converted with `bzop2bzvc`:
```wl
snegfermionoperators[c];
bz = qsbasis[{c[1], c[2]}];
Expand[transformtoLRvc[bz, {c[1], c[2]}, vacuum[]]] === Expand[bzop2bzvc[transformtoLR[bz, {c[1], c[2]}]]]
```
