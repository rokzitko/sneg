---
keywords: spin matrices, z component, spin projection, spin-s representation
see also: spinmatrixX, spinmatrixY, spinmatrixP, spinmatrixM, spinketbraZ
---
# spinmatrixZ

## Usage
- `spinmatrixZ[s]` gives the $(2s+1) \times (2s+1)$ matrix of the spin component $S^z$ in the spin-*s* representation.

## Details
- The basis is $|s, m\rangle$ with $m = s, s-1, \ldots, -s$; $S^z = \mathrm{diag}(s, s-1, \ldots, -s)$. See `spinmatrixX` for the conventions.
- *s* must be an integer or a half-integer; otherwise the expression remains unevaluated.

## Examples
### Basic Examples
```wl
spinmatrixZ[1] // MatrixForm
```
```wl
spinmatrixZ[3/2] // MatrixForm
```

### Properties & Relations
$[S^z, S^+] = S^+$:
```wl
spinmatrixZ[3/2] . spinmatrixP[3/2] - spinmatrixP[3/2] . spinmatrixZ[3/2] == spinmatrixP[3/2]
```
