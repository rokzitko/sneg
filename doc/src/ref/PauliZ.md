---
keywords: Pauli matrices, sigma_z, spin-1/2
see also: PauliX, PauliY, PauliPlus, PauliMinus, spinmatrixX, spinxyz, matrixhop, su3g
---
# PauliZ

## Usage
- `PauliZ` is the Pauli matrix $\sigma_z$ = `{{1, 0}, {0, -1}}`.

## Details
- Rows and columns are ordered as `{UP, DO}`. See `PauliX` for the conventions and the use of the Pauli matrices in SNEG.
## Examples
### Basic Examples
```wl
MatrixForm[PauliZ]
```

### Properties & Relations
```wl
{spinmatrixZ[1/2] == PauliZ/2, PauliZ . PauliZ == IdentityMatrix[2]}
```
