---
keywords: Dirac notation, bra, bra-ket, dual vector, scalar product
see also: ket, braketrule, nc, conj, ap, spinbra
---
# bra

## Usage
- `bra[i]` represents the Dirac bra $\langle i|$.
- `bra[i, j, ...]` represents the bra $\langle i, j, \ldots|$ of a product state; entries `Null` mark subsystems that the bra does not refer to.

## Details
- `bra[i]` is the Hermitian conjugate of `ket[i]`: `conj[ket[i]]` is `bra[i]`.
- In `nc` products, `nc[bra[i], ket[j]]` is evaluated by `braketrule[bra[i], ket[j]]` (by default $\delta_{ij}$) if the bra and the ket refer to the same subsystems (the same pattern of `Null` entries). Bras of complementary subsystems are combined: `nc[bra[a, Null], bra[Null, b]]` is `bra[a, b]`.
- A bra acting on a state `vc[..., ket[j]]` in the occupation number representation gives the braket with the ket of the state; see `ap`.
- All other rules, examples, and applications are described on the page of `ket`.

## Examples
### Basic Examples
```wl
{nc[bra[1], ket[1]], nc[bra[1], ket[2]], nc[bra[a], ket[b]]}
```
```wl
conj[ket[1/2]]
```

### Scope
Bras of subsystems:
```wl
nc[bra[x, Null], bra[Null, y]]
```
---
A bra applied to a state with a ket in the occupation number representation:
```wl
snegfermionoperators[c];
makebasis[{c[]}];
ap[nc[c[CR, UP], bra[1/2]], vc[0, 0, ket[1/2]]]
```

### Properties & Relations
The expectation value of a ket-bra operator, $\langle 1|(|0\rangle\langle 1| + |1\rangle\langle 1|)|1\rangle$:
```wl
nc[bra[1], nc[ket[0], bra[1]] + nc[ket[1], bra[1]], ket[1]]
```
