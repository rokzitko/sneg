---
keywords: Pauli matrices, sigma_plus, spin raising, spin-1/2
see also: PauliX, PauliY, PauliZ, PauliMinus, spinmatrixX, spinxyz, matrixhop, su3g
---
# PauliPlus

## Usage
- `PauliPlus` is the Pauli matrix $\sigma_x + i\sigma_y$ = `{{0, 2}, {0, 0}}`, twice the spin-raising matrix $\sigma^+$.

## Details
- Rows and columns are ordered as `{UP, DO}`. See `PauliX` for the conventions and the use of the Pauli matrices in SNEG.
- Note the normalization: `PauliPlus` and `PauliMinus` have the matrix element 2, so that $S^\pm = $ `PauliPlus/2`, `PauliMinus/2` are the spin-1/2 raising and lowering operators.

## Examples
### Basic Examples
```wl
MatrixForm[PauliPlus]
```

### Properties & Relations
```wl
{spinmatrixP[1/2] == PauliPlus/2, PauliPlus == PauliX + I PauliY}
```
