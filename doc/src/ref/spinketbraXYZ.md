---
keywords: bra-ket representation, spin vector, spin operators
see also: spinketbraX, spinketbraOPS, spinketbraspinspin, spinketbrarules
---
# spinketbraXYZ

## Usage
- `spinketbraXYZ[s]` gives the list $\{S^x, S^y, S^z\}$ of the spin-*s* operators in the bra-ket representation.
- `spinketbraXYZ[s, pos]` places the spin in the slots specified by the position array *pos*.

## Details
- `spinketbraXYZ[s, pos]` is `{spinketbraX[s, pos], spinketbraY[s, pos], spinketbraZ[s, pos]}`; see `spinketbraX`.

## Examples
### Basic Examples
```wl
spinketbraXYZ[1/2]
```

### Properties & Relations
$\mathbf S^2 = s(s+1)$ times the identity:
```wl
{sx, sy, sz} = spinketbraXYZ[1];
Expand[nc[sx, sx] + nc[sy, sy] + nc[sz, sz] - 2 spinketbraI[1]]
```
