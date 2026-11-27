---
keywords: Pauli matrices, sigma_y, spin-1/2
see also: PauliX, PauliZ, PauliPlus, PauliMinus, spinmatrixX, spinxyz, matrixhop, su3g
---
# PauliY

## Usage
- `PauliY` is the Pauli matrix $\sigma_y$ = `{{0, -I}, {I, 0}}`.

## Details
- Rows and columns are ordered as `{UP, DO}`. See `PauliX` for the conventions and the use of the Pauli matrices in SNEG.
## Examples
### Basic Examples
```wl
MatrixForm[PauliY]
```

### Properties & Relations
```wl
{spinmatrixY[1/2] == PauliY/2, PauliY . PauliY == IdentityMatrix[2]}
```
