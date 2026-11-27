---
keywords: orthogonalization, Gram-Schmidt, orthonormal basis, linear independence, creation operator representation
see also: scalarproductop, decomposeop, normop, orthogvc
---
# orthogop

## Usage
- `orthogop[vecs, basis]` orthonormalizes the states *vecs* in the creation-operator representation, using the orthonormal basis states *basis*.

## Details
- Every state in *vecs* is decomposed in *basis* with `decomposeop` (scalar products `scalarproductop`), the coefficient vectors are orthonormalized in the order of *vecs* (Gram-Schmidt), and the resulting states are expanded in *basis*.
- Linearly dependent states are dropped, so the result spans the same subspace as *vecs* and may be shorter.
- *basis* must be a list of orthonormal states that spans *vecs*. The vacuum is the one set by `ordering`.

## Examples
### Basic Examples
Orthonormalization of the two-electron states with one electron on each site:
```wl
snegfermionoperators[c];
b = {nc[c[CR, 1, UP], c[CR, 2, DO]], nc[c[CR, 1, DO], c[CR, 2, UP]]};
orthogop[{b[[1]] - b[[2]], b[[1]] + b[[2]], b[[1]]}, b]
```
The third state is a linear combination of the first two and is dropped.

### Scope
The order of the states determines the result:
```wl
snegfermionoperators[c];
b = {nc[c[CR, 1, UP], c[CR, 2, DO]], nc[c[CR, 1, DO], c[CR, 2, UP]]};
orthogop[{b[[1]], b[[1]] - b[[2]]}, b]
```

### Applications
Construction of the $S_z = 0$ triplet state by applying the total spin-lowering operator to $c^\dagger_{1\uparrow} c^\dagger_{2\uparrow}|0\rangle$, with normalization:
```wl
snegfermionoperators[c];
b = {nc[c[CR, 1, UP], c[CR, 2, DO]], nc[c[CR, 1, DO], c[CR, 2, UP]]};
v = nc[spinminus[c[1]] + spinminus[c[2]], c[CR, 1, UP], c[CR, 2, UP]];
orthogop[{v}, b]
```
