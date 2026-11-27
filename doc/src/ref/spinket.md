---
keywords: spin kets, spin states, basis states, bra-ket representation
see also: spinbra, spinbasis, spinketbraX, ket, bra, braketrule
---
# spinket

## Usage
- `spinket[s]` gives the list of the spin-*s* basis kets `ket[m]`, $m = s, s-1, \ldots, -s$.
- `spinket[s, pos]` places the spin projection in the slots of the kets specified by the position array *pos*; the other slots are `Null`.

## Details
- The kets are orthonormal, $\langle m|m'\rangle = \delta_{mm'}$ (see `braketrule`). `spinbra` gives the conjugate bras, and `spinbasis` the kets in the form of a basis with the quantum number $S_z$.
- The position array is a list of 0 and 1; e.g. `{1, 0}` gives `ket[m, Null]` for the first of two spins. See `spinketbraX`.
- *s* must be an integer or a half-integer.

## Examples
### Basic Examples
```wl
spinket[1]
```
```wl
spinket[1/2, {0, 1}]
```

### Properties & Relations
The bras are the conjugates of the kets:
```wl
conj[spinket[3/2]] == spinbra[3/2]
```
---
Orthonormality:
```wl
Outer[nc, spinbra[1], spinket[1]]
```
