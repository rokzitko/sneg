---
keywords: bra-ket representation, identity operator, spin, tensor product
see also: spinketbraX, spinketbrarules, spinket, spinbasis
---
# spinketbraI

## Usage
- `spinketbraI[s]` gives the identity operator $\sum_m |m\rangle\langle m|$ of the spin-*s* space in the bra-ket representation.
- `spinketbraI[s, pos]` places it in the slots specified by the position array *pos*.

## Details
- The identity is needed to extend an operator that acts on one slot of a multi-spin ket to the full ket, e.g. $S^z_1 \otimes 1_2$ = `nc[spinketbraZ[s1, {1, 0}], spinketbraI[s2, {0, 1}]]`. `spinketbrarules` includes these factors automatically.
- See `spinketbraX` for the position arrays.

## Examples
### Basic Examples
```wl
spinketbraI[1]
```
The identity of the second slot of two spins 1/2:
```wl
spinketbraI[1/2, {0, 1}]
```

### Properties & Relations
$S^z_1 \otimes 1_2$ acting on a two-spin ket:
```wl
nc[spinketbraZ[1/2, {1, 0}], spinketbraI[1/2, {0, 1}], ket[-1/2, 1/2]]
```
