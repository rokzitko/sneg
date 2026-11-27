---
keywords: spin lowering matrix, ladder operator, spin-s representation
see also: spinmatrixP, spinmatrixX, spinmatrixY, spinmatrixZ, spinketbraM
---
# spinmatrixM

## Usage
- `spinmatrixM[s]` gives the $(2s+1) \times (2s+1)$ matrix of the spin-lowering operator $S^-$ in the spin-*s* representation.

## Details
- The basis is $|s, m\rangle$ with $m = s, s-1, \ldots, -s$; $\langle m-1|S^-|m\rangle = \sqrt{s(s+1) - m(m-1)}$, so the nonzero elements are on the first subdiagonal. See `spinmatrixX` for the conventions.
- *s* must be an integer or a half-integer; otherwise the expression remains unevaluated.

## Examples
### Basic Examples
```wl
spinmatrixM[1] // MatrixForm
```
```wl
spinmatrixM[3/2] // MatrixForm
```

### Properties & Relations
$S^- = (S^+)^\dagger$:
```wl
spinmatrixM[5/2] == ConjugateTranspose[spinmatrixP[5/2]]
```
