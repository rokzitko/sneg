---
keywords: spin ket, spin projection, SU(2) basis, NRG Ljubljana
see also: fixspin, spinket, SU2basistensorproduct
---
# fixspinket

## Usage
- `fixspinket[ket[s], s, sz]` gives the spin ket `ket[sz]` of the multiplet with total spin *s*, obtained from the highest-weight ket `ket[s]`.

## Details
- `fixspinket` is the counterpart of `fixspin` for spin kets: it shifts the spin projection in single-slot kets `ket[m]` by $sz - s$. Kets with several slots are returned unchanged.
- `fixspinket[k, s, s]` is *k*. For $sz > s$ the expression remains unevaluated.
- `fixspin` and `fixspinket` are used as arguments of `SU2basistensorproduct` to combine a fermionic basis with an impurity spin into a basis with good total spin, as done in NRG Ljubljana.

## Examples
### Basic Examples
```wl
{fixspinket[ket[1], 1, 0], fixspinket[ket[1], 1, -1], fixspinket[ket[1/2], 1/2, -1/2]}
```

### Possible Issues
Only kets with a single slot are shifted:
```wl
fixspinket[ket[1/2, Null], 1/2, -1/2]
```
