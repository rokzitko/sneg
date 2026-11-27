---
keywords: spin raising matrix, ladder operator, spin-s representation
see also: spinmatrixM, spinmatrixX, spinmatrixY, spinmatrixZ, spinketbraP
---
# spinmatrixP

## Usage
- `spinmatrixP[s]` gives the $(2s+1) \times (2s+1)$ matrix of the spin-raising operator $S^+$ in the spin-*s* representation.

## Details
- The basis is $|s, m\rangle$ with $m = s, s-1, \ldots, -s$; $\langle m+1|S^+|m\rangle = \sqrt{s(s+1) - m(m+1)}$, so the nonzero elements are on the first superdiagonal. See `spinmatrixX` for the conventions.
- *s* must be an integer or a half-integer; otherwise the expression remains unevaluated.

## Examples
### Basic Examples
```wl
spinmatrixP[1] // MatrixForm
```
```wl
spinmatrixP[3/2] // MatrixForm
```

### Properties & Relations
$[S^+, S^-] = 2 S^z$:
```wl
spinmatrixP[2] . spinmatrixM[2] - spinmatrixM[2] . spinmatrixP[2] == 2 spinmatrixZ[2]
```
