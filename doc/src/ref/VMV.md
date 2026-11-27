---
keywords: vector-matrix-vector product, bilinear form, spinor, Pauli matrices, quadratic Hamiltonian, Nambu spinor
see also: inner, outer, mcross, nc, spinxyz, PauliMatrix
---
# VMV

## Usage
- `VMV[v1, m, v2]` gives the product $\sum_{ij} (v_1)_i\, m_{ij}\, (v_2)_j$ of the vectors *v1*, *v2* and the matrix *m*, with the products formed by `nc`.

## Details
- `VMV[v1, m, v2]` is `inner[inner[v1, m], v2]`. The operator order $v_{1,i}$, $v_{2,j}$ is preserved.
- With $v_1 = (c^\dagger_1, c^\dagger_2, \ldots)$ and $v_2 = (c_1, c_2, \ldots)$, `VMV[v1, h, v2]` is the bilinear form $\sum_{ij} c^\dagger_i h_{ij} c_j$, e.g. a tight-binding Hamiltonian or the spin density $\frac12 \sum_{\sigma\sigma'} c^\dagger_\sigma \boldsymbol\sigma_{\sigma\sigma'} c_{\sigma'}$.
- More generally, the last index of *v1* is contracted with the first index of *m*, and the last index of *m* with the first index of *v2*. *m* can thus be a list of matrices with *v1* a list of operators, as in $\sum_a \psi_a \sigma^a$.

## Examples
### Basic Examples
```wl
VMV[{a1, a2}, {{m11, m12}, {m21, m22}}, {b1, b2}]
```
The spin of a level, $S^z = \frac12 (c^\dagger_\uparrow c_\uparrow - c^\dagger_\downarrow c_\downarrow)$:
```wl
snegfermionoperators[c];
VMV[{c[CR, UP], c[CR, DO]}, PauliMatrix[3]/2, {c[AN, UP], c[AN, DO]}]
```

### Applications
All spin components from the Pauli matrices agree with `spinxyz`:
```wl
snegfermionoperators[c];
Expand[VMV[{c[CR, UP], c[CR, DO]}, PauliMatrix[#]/2, {c[AN, UP], c[AN, DO]}] & /@ {1, 2, 3}] == Expand[spinxyz[c[]]]
```
---
Tight-binding Hamiltonian of a three-site ring with hopping $t$ and spin up:
```wl
snegfermionoperators[c];
snegrealconstants[t];
cr = {c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, UP]};
an = {c[AN, 1, UP], c[AN, 2, UP], c[AN, 3, UP]};
H = VMV[cr, -t {{0, 1, 1}, {1, 0, 1}, {1, 1, 0}}, an]
```
Its one-electron spectrum follows from `matrixrepresentationop`:
```wl
Eigenvalues[matrixrepresentationop[H, cr]]
```
