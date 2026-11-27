---
keywords: Pauli matrices, sigma_minus, spin lowering, spin-1/2
see also: PauliX, PauliY, PauliZ, PauliPlus, spinmatrixX, spinxyz, matrixhop, su3g
---
# PauliMinus

## Usage
- `PauliMinus` is the Pauli matrix $\sigma_x - i\sigma_y$ = `{{0, 0}, {2, 0}}`, twice the spin-lowering matrix $\sigma^-$.

## Details
- Rows and columns are ordered as `{UP, DO}`. See `PauliX` for the conventions and the use of the Pauli matrices in SNEG.
- Note the normalization: `PauliPlus` and `PauliMinus` have the matrix element 2, so that $S^\pm = $ `PauliPlus/2`, `PauliMinus/2` are the spin-1/2 raising and lowering operators.

## Examples
### Basic Examples
```wl
MatrixForm[PauliMinus]
```

### Properties & Relations
```wl
{spinmatrixM[1/2] == PauliMinus/2, PauliMinus == PauliX - I PauliY}
```
