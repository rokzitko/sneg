---
keywords: spin matrices, spin-s representation, angular momentum matrices, Pauli matrices, irreducible representation of su(2)
see also: spinmatrixY, spinmatrixZ, spinmatrixP, spinmatrixM, spinketbraX, spinclustermatrices, PauliMatrix
---
# spinmatrixX

## Usage
- `spinmatrixX[s]` gives the $(2s+1) \times (2s+1)$ matrix of the spin component $S^x$ in the spin-*s* representation.

## Details
- The basis is $|s, m\rangle$ with $m = s, s-1, \ldots, -s$ (row and column 1 correspond to $m = s$). The matrices of the spin-*s* representation are
  $$\langle m'|S^z|m\rangle = m\, \delta_{m'm}, \qquad \langle m'|S^\pm|m\rangle = \sqrt{s(s+1) - m(m \pm 1)}\; \delta_{m', m\pm1},$$
  with the standard (Condon-Shortley) phase convention: all matrix elements of $S^\pm$ are real and non-negative. $S^x = (S^+ + S^-)/2$ and $S^y = (S^+ - S^-)/(2i)$.
- The five functions `spinmatrixX`, `spinmatrixY`, `spinmatrixZ`, `spinmatrixP` ($S^+$), `spinmatrixM` ($S^-$) are defined for integer and half-integer *s* (also $s = 0$); for other arguments they remain unevaluated.
- For $s = 1/2$, the matrices are half the Pauli matrices: `spinmatrixX[1/2] == PauliX/2`.
- These matrices define the spin-*s* fermion operators of `spinxyz`, the bra-ket operators `spinketbraX`, ..., the reduction of products of spin operators with a declared size, and the matrices of `spinclustermatrices`.

## Examples
### Basic Examples
```wl
spinmatrixX[1] // MatrixForm
```
All components for spin 1/2:
```wl
MatrixForm /@ {spinmatrixX[1/2], spinmatrixY[1/2], spinmatrixZ[1/2], spinmatrixP[1/2], spinmatrixM[1/2]}
```

### Scope
Spin 3/2:
```wl
spinmatrixX[3/2] // MatrixForm
```

### Applications
Single-ion anisotropy $D (S^z)^2$ and a transverse field $B S^x$ for a spin 1:
```wl
snegrealconstants[Dz, B];
Eigenvalues[Dz spinmatrixZ[1] . spinmatrixZ[1] + B spinmatrixX[1]]
```

### Properties & Relations
For spin 1/2 the matrices are half the Pauli matrices:
```wl
{spinmatrixX[1/2], spinmatrixY[1/2], spinmatrixZ[1/2]} == {PauliX, PauliY, PauliZ}/2
```
---
The su(2) commutation relations and the Casimir operator $\mathbf S^2 = s(s+1)$ for spin 2:
```wl
{sx, sy, sz} = {spinmatrixX[2], spinmatrixY[2], spinmatrixZ[2]};
{sx . sy - sy . sx == I sz, sx . sx + sy . sy + sz . sz == 2 (2 + 1) IdentityMatrix[5]}
```
---
`spinmatrixX` gives the same matrices as `spinclustermatrices` for a single spin:
```wl
snegspinoperators[{Q, 3/2}];
spinclustermatrices[Q[1, 1], {Q[1]}, "Blocks" -> False] == spinmatrixX[3/2]
```

### Possible Issues
The spin must be an integer or a half-integer:
```wl
spinmatrixX[1/3]
```
