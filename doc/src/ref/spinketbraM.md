---
keywords: bra-ket representation, spin operator, spin lowering operator, ladder operator
see also: spinketbraX, spinmatrixM, spinketbraP, spinketbrarules, spinket
---
# spinketbraM

## Usage
- `spinketbraM[s]` gives the lowering operator $S^-$ of the spin-*s* operator in the bra-ket representation.
- `spinketbraM[s, pos]` places the spin in the slots specified by the position array *pos*.

## Details
- The result is $\sum_{mm'} |m\rangle M_{mm'} \langle m'|$, written with `nc[ket[m], bra[m']]`, with the matrix `spinmatrixM[s]`. See `spinketbraX` for the position arrays and the use of the bra-ket representation.
- *s* must be an integer or a half-integer.

## Examples
### Basic Examples
```wl
spinketbraM[1/2]
```
```wl
nc[spinketbraM[1], ket[0]]
```

### Properties & Relations
```wl
conj[spinketbraP[1]] == spinketbraM[1]
```
