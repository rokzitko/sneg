---
keywords: spin-dependent hopping, spin-orbit coupling, Rashba, Dresselhaus, SU(2) gauge field, spin-flip hopping, hopping matrix, Pauli matrices, tight-binding
see also: hop, spinfliphop, genhop, hopphi, PauliX, PauliY, PauliZ, blochhamiltonian, clusterhop, fourier
---
# matrixhop

## Usage
- `matrixhop[T, a[i], b[j]]` gives the spin-dependent hopping operator $\sum_{\sigma\sigma'} a^\dagger_{i\sigma} T_{\sigma\sigma'} b_{j\sigma'} + \mathrm{h.c.}$ with the $2\times2$ matrix *T*.

## Details
- In matrix notation, `matrixhop[T, a[i], b[j]]` is $a_i^\dagger T\, b_j + b_j^\dagger T^\dagger a_i$ with the spinors $a_i = (a_{i\uparrow}, a_{i\downarrow})^T$ and $b_j = (b_{j\uparrow}, b_{j\downarrow})^T$. Rows and columns of *T* are ordered as `{UP, DO}`.
- *T* is a dense matrix or a `SparseArray`. Its entries can be numbers, declared constants, or undeclared symbols; all entries are treated as complex scalars, and the Hermitian conjugate is formed with `Conjugate`. Constants declared with `snegrealconstants` are real.
- Special cases: `IdentityMatrix[2]` gives `hop`, `PauliX` gives `spinfliphop`, `z IdentityMatrix[2]` gives `genhop[z, a[i], b[j]]`, and `PauliZ` gives $\mathrm{hop}_\uparrow - \mathrm{hop}_\downarrow$.
- A spin-orbit coupled bond with hopping $t$ and spin-orbit amplitude $\boldsymbol\alpha$ has $T = -t\,\mathbf{1} + i\, \boldsymbol\alpha\cdot\boldsymbol\sigma$; a unitary $T = -t\, e^{i\theta\, \mathbf n\cdot\boldsymbol\sigma}$ describes a spin rotation about $\mathbf n$ by the angle $-2\theta$ along the bond (SU(2) link variable; $e^{-i\alpha\, \mathbf n\cdot\boldsymbol\sigma/2}$ is the rotation by $\alpha$).
- The endpoints are spin-1/2 fermion levels given without the creation/annihilation and spin indexes, as in `hop`, or abstract orbitals: pure functions `fn` with `fn[CR, σ]` the creation operator of the orbital; the annihilation operator is `conj[fn[CR, σ]]`.
- For symbolic matrices, *T* is decomposed as $T = \sum_{\mu=0}^{3} t_\mu \sigma_\mu$ with $\sigma_0 = \mathbf{1}$, $t_\mu = \mathrm{Tr}(T\sigma_\mu)/2$. The coefficients are simplified with `ExpToTrig` and `Simplify` under the current `$Assumptions`, and the decomposition is used if it is shorter than the matrix entries. Spin rotations given by `MatrixExp` thus give trigonometric coefficients. Numeric matrices and matrices with inexact numbers are used directly.
- If *T* is not a $2\times2$ matrix, the message `matrixhop::matrix` is issued and `$Failed` is returned; if an endpoint is not a spin-1/2 fermion level or a pure function, `matrixhop::op` is issued and `$Failed` is returned.
- `matrixhop` is not `Listable`, because the matrix is a single argument.

## Examples
### Basic Examples
Spin-orbit hopping $i\alpha\, c_1^\dagger \sigma_y c_2 + \mathrm{h.c.}$, which flips the spin:
```wl
snegfermionoperators[c];
snegrealconstants[alpha];
matrixhop[I alpha PauliY, c[1], c[2]]
```

### Scope
A general complex hopping matrix. Undeclared entries are complex scalars:
```wl
snegfermionoperators[c];
matrixhop[{{t11, t12}, {t21, t22}}, c[1], c[2]]
```
---
Sparse matrices:
```wl
snegrealconstants[t];
matrixhop[SparseArray[{{1, 2} -> t}, {2, 2}], c[1], c[2]]
```
---
A spin rotation along the bond, $T = t\, e^{i\theta\sigma_y}$, is simplified to $t(\cos\theta\, \mathbf{1} + i \sin\theta\, \sigma_y)$:
```wl
snegrealconstants[t, theta];
matrixhop[t MatrixExp[I theta PauliY], c[1], c[2]]
```
---
Hopping between abstract orbitals, given as pure functions of the type and spin indexes, or between an abstract orbital and an ordinary level:
```wl
snegfermionoperators[c, d];
bond = (c[#1, 1, #2] + c[#1, 2, #2])/Sqrt[2] &;
matrixhop[PauliZ, bond, d[]]
```

### Applications
Rashba chain. With $T = -t\,\mathbf{1} + i\alpha\sigma_y$ on every bond, the Bloch Hamiltonian is $h(k) = -2t\cos k - 2\alpha \sin k\, \sigma_y$, and the bands $-2t\cos k \mp 2\alpha\sin k$ are spin split:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t, alpha];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = sum[matrixhop[-t IdentityMatrix[2] + I alpha PauliY, c[R], c[R + 1]], {R}];
hk = ExpToTrig[blochphaseexp[blochhamiltonian[fourier[H, c -> ck, R -> k], {ck}, k]]]
```
```wl
Eigenvalues[hk]
```
---
SU(2) flux through a ring. On a three-site ring with $T = -t\, e^{i\theta\sigma_y}$ on each bond, electrons with $\sigma_y = \pm1$ see the Peierls phases $\pm\theta$ per bond, and the levels are $-2t\cos(2\pi m/3 \pm \theta)$:
```wl
snegfermionoperators[c];
snegrealconstants[t, theta];
H = Sum[matrixhop[-t MatrixExp[I theta PauliY], c[j], c[Mod[j, 3] + 1]], {j, 3}];
states = Flatten[Table[c[CR, j, s], {j, 3}, {s, {UP, DO}}]];
Simplify[Eigenvalues[matrixrepresentationop[H, states]]]
```
On an open chain, the same spin rotation can be removed by a local SU(2) gauge transformation, and the spectrum does not depend on $\theta$:
```wl
H = Sum[matrixhop[-t MatrixExp[I theta PauliY], c[j], c[j + 1]], {j, 2}];
Simplify[Eigenvalues[matrixrepresentationop[H, states]]]
```

### Properties & Relations
The single-particle matrix in the basis $a^\dagger_\uparrow, a^\dagger_\downarrow, b^\dagger_\uparrow, b^\dagger_\downarrow$ has the blocks $T$ (upper right) and $T^\dagger$ (lower left):
```wl
snegfermionoperators[a, b];
snegcomplexconstants[t11, t12, t21, t22];
T = {{t11, t12}, {t21, t22}};
matrixrepresentationop[matrixhop[T, a[], b[]], {a[CR, UP], a[CR, DO], b[CR, UP], b[CR, DO]}] == ArrayFlatten[{{0, T}, {ConjugateTranspose[T], 0}}]
```
---
Exchanging the endpoints replaces $T$ by $T^\dagger$:
```wl
Expand[matrixhop[T, a[], b[]] - matrixhop[ConjugateTranspose[T], b[], a[]]]
```
---
Special cases:
```wl
snegfermionoperators[c];
{matrixhop[IdentityMatrix[2], c[1], c[2]] == hop[c[1], c[2]], matrixhop[PauliX, c[1], c[2]] == spinfliphop[c[1], c[2]], Expand[matrixhop[PauliZ, c[1], c[2]] - hop[c[1], c[2], UP] + hop[c[1], c[2], DO]]}
```

### Possible Issues
With equal endpoints, the Hermitian conjugate is added on the same level, so that the result is $a^\dagger (T + T^\dagger) a$. For an on-site Zeeman term $B S_z = \frac{B}{2}\, a^\dagger\sigma_z a$, use $T = \frac{B}{4}\sigma_z$:
```wl
snegfermionoperators[c];
snegrealconstants[B];
Expand[{matrixhop[B/2 PauliZ, c[], c[]] - B spinz[c[]], matrixhop[B/4 PauliZ, c[], c[]] - B spinz[c[]]}]
```
---
The matrix must be $2\times2$:
```wl
matrixhop[{{1, 0, 0}}, c[1], c[2]]
```
---
The endpoints must be spin-1/2 fermions:
```wl
snegspinlessfermionoperators[f];
matrixhop[IdentityMatrix[2], f[1], c[2]]
```
