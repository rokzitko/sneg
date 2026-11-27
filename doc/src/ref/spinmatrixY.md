---
keywords: spin matrices, y component, spin-s representation
see also: spinmatrixX, spinmatrixZ, spinmatrixP, spinmatrixM, spinketbraY
---
# spinmatrixY

## Usage
- `spinmatrixY[s]` gives the $(2s+1) \times (2s+1)$ matrix of the spin component $S^y$ in the spin-*s* representation.

## Details
- The basis is $|s, m\rangle$ with $m = s, s-1, \ldots, -s$; $S^y = (S^+ - S^-)/(2i)$ is imaginary and antisymmetric. See `spinmatrixX` for the conventions.
- *s* must be an integer or a half-integer; otherwise the expression remains unevaluated.

## Examples
### Basic Examples
```wl
spinmatrixY[1] // MatrixForm
```
```wl
spinmatrixY[3/2] // MatrixForm
```

### Properties & Relations
$S^y = (S^+ - S^-)/(2i)$:
```wl
spinmatrixY[2] == (spinmatrixP[2] - spinmatrixM[2])/(2 I)
```
