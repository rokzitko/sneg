---
keywords: spin lowering, occupation-number representation, spin multiplet
see also: spindown, makebasis, ops2vc, vc2ops, spinminus
---
# spindownvc

## Usage
- `spindownvc[v]` applies the total spin-lowering operator $S^- = \sum_j c^\dagger_{j\downarrow} c_{j\uparrow}$ to the state *v* in the occupation-number representation and normalizes the result.

## Details
- *v* is a vector `vc[n1, n2, ...]` or a linear combination of such vectors, in the basis declared with `makebasis`. The occupation numbers are taken in consecutive pairs (spin up, spin down) of the levels, the order of `makebasis` for spin-1/2 fermions.
- Each pair `1, 0` (spin up) is replaced by `0, 1` (spin down). Since the up and down operators of a level are adjacent in the basis, no fermion sign arises. The result is divided by its norm; if $S^- v = 0$, it is 0.
- A spin ket at the end of the vector, `vc[..., ket[m]]` (see `QSZaddspinket`), is not lowered: only the fermionic part is acted upon, and the result is 0 if that vanishes.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
v = ops2vc[nc[c[CR, 1, UP], c[CR, 2, UP]]]
```
```wl
NestList[spindownvc, v, 3]
```

### Properties & Relations
Agreement with `spindown`:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vc2ops[spindownvc[vc[1, 0, 1, 0]]] == spindown[nc[c[CR, 1, UP], c[CR, 2, UP]]]
```
