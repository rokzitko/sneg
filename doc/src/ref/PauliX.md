---
keywords: Pauli matrices, sigma_x, spin-1/2, two-level system, qubit
see also: PauliY, PauliZ, PauliPlus, PauliMinus, spinmatrixX, spinxyz, matrixhop, su3g
---
# PauliX

## Usage
- `PauliX` is the Pauli matrix $\sigma_x$ = `{{0, 1}, {1, 0}}`.

## Details
- The Pauli matrices `PauliX`, `PauliY`, `PauliZ` are $2\times2$ matrices in the basis $(\uparrow, \downarrow)$, i.e. the rows and columns are ordered as `{UP, DO}`. `PauliPlus` and `PauliMinus` are $\sigma_x \pm i\sigma_y$.
- The spin-1/2 operators are $S^a = \sigma_a/2$; `spinmatrixX[1/2]` is `PauliX/2`.
- The Pauli matrices are used to define the spin operators of fermion levels, $S^a = \frac{1}{2} \sum_{\sigma\sigma'} c^\dagger_\sigma (\sigma_a)_{\sigma\sigma'} c_{\sigma'}$ (`spinxyz`), the isospin operators, and spin-dependent hopping matrices in `matrixhop`.
- `PauliX` is a variable holding a matrix, not a function.

## Examples
### Basic Examples
```wl
MatrixForm /@ {PauliX, PauliY, PauliZ}
```

### Properties & Relations
The algebra $\sigma_a \sigma_b = \delta_{ab} + i \epsilon_{abc} \sigma_c$:
```wl
{PauliX . PauliY == I PauliZ, PauliY . PauliZ == I PauliX, PauliZ . PauliX == I PauliY, PauliX . PauliX == IdentityMatrix[2]}
```
---
The spin operators of a fermion level are built with the Pauli matrices:
```wl
snegfermionoperators[c];
Expand[1/2 VMV[{c[CR, UP], c[CR, DO]}, PauliX, {c[AN, UP], c[AN, DO]}] - spinx[c[]]]
```
---
Spin-1/2 matrices:
```wl
spinmatrixX[1/2] == PauliX/2
```
---
`matrixhop` with $\sigma_x$ is the spin-flip hopping:
```wl
matrixhop[PauliX, c[1], c[2]] == spinfliphop[c[1], c[2]]
```
