---
keywords: bra-ket representation, spin operator, y component
see also: spinketbraX, spinmatrixY, spinketbrarules, spinket
---
# spinketbraY

## Usage
- `spinketbraY[s]` gives the $y$ component of the spin-*s* operator in the bra-ket representation.
- `spinketbraY[s, pos]` places the spin in the slots specified by the position array *pos*.

## Details
- The result is $\sum_{mm'} |m\rangle M_{mm'} \langle m'|$, written with `nc[ket[m], bra[m']]`, with the matrix `spinmatrixY[s]`. See `spinketbraX` for the position arrays and the use of the bra-ket representation.
- *s* must be an integer or a half-integer.

## Examples
### Basic Examples
```wl
spinketbraY[1/2]
```
```wl
nc[spinketbraY[1/2], ket[1/2]]
```

### Properties & Relations
```wl
matrixrepresentationop[spinketbraY[1], spinket[1]] == spinmatrixY[1]
```
