---
keywords: spin bras, spin states, bra-ket representation
see also: spinket, spinbasis, spinketbraX, bra, ket
---
# spinbra

## Usage
- `spinbra[s]` gives the list of the spin-*s* basis bras `bra[m]`, $m = s, s-1, \ldots, -s$.
- `spinbra[s, pos]` places the spin projection in the slots specified by the position array *pos*.

## Details
- `spinbra[s, pos]` is `conj[spinket[s, pos]]`; see `spinket`.

## Examples
### Basic Examples
```wl
spinbra[1/2]
```

### Applications
The projector onto $m = 1/2$ written with bras and kets:
```wl
nc[spinket[1/2][[1]], spinbra[1/2][[1]]]
```
