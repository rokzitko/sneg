---
keywords: bra-ket representation, spin operator, z component, spin projection
see also: spinketbraX, spinmatrixZ, spinketbrarules, spinket
---
# spinketbraZ

## Usage
- `spinketbraZ[s]` gives the $z$ component of the spin-*s* operator in the bra-ket representation.
- `spinketbraZ[s, pos]` places the spin in the slots specified by the position array *pos*.

## Details
- The result is $\sum_{mm'} |m\rangle M_{mm'} \langle m'|$, written with `nc[ket[m], bra[m']]`, with the matrix `spinmatrixZ[s]`, $M_{mm'} = m\,\delta_{mm'}$. See `spinketbraX` for the position arrays and the use of the bra-ket representation.
- *s* must be an integer or a half-integer.

## Examples
### Basic Examples
```wl
spinketbraZ[1/2]
```
```wl
spinketbraZ[1]
```

### Properties & Relations
```wl
nc[spinketbraZ[3/2], ket[-1/2]]
```
