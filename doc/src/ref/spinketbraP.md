---
keywords: bra-ket representation, spin operator, spin raising operator, ladder operator
see also: spinketbraX, spinmatrixP, spinketbraM, spinketbrarules, spinket
---
# spinketbraP

## Usage
- `spinketbraP[s]` gives the raising operator $S^+$ of the spin-*s* operator in the bra-ket representation.
- `spinketbraP[s, pos]` places the spin in the slots specified by the position array *pos*.

## Details
- The result is $\sum_{mm'} |m\rangle M_{mm'} \langle m'|$, written with `nc[ket[m], bra[m']]`, with the matrix `spinmatrixP[s]`. See `spinketbraX` for the position arrays and the use of the bra-ket representation.
- *s* must be an integer or a half-integer.

## Examples
### Basic Examples
```wl
spinketbraP[1/2]
```
```wl
nc[spinketbraP[1], ket[0]]
```

### Properties & Relations
```wl
Expand[nc[spinketbraP[1], spinketbraM[1]] - nc[spinketbraM[1], spinketbraP[1]] - 2 spinketbraZ[1]]
```
