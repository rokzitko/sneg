---
keywords: orthogonalization, Gram-Schmidt, orthonormal basis, linear independence, occupation number representation, projection
see also: decomposevc, orthogop, orthogbasisvc, remove1state, transformbasis, scalarproductvc, normvc, Orthogonalize
---
# orthogvc

## Usage
- `orthogvc[vecs, basis]` gives an orthonormal basis of the space spanned by the states *vecs*, expressed in the orthonormal states *basis*; all states are in the occupation number representation.

## Details
- Each state of *vecs* is decomposed into its components $\langle b_j | v\rangle$ with respect to the states $b_j$ of *basis* (`decomposevc`). The coefficient vectors are orthonormalized with the Gram-Schmidt procedure (`Orthogonalize` with the inner product $\sum_j a_j^* b_j$ and `Simplify`), vectors that are proven to be zero are dropped, and the results are converted back to states.
- The result is a list of orthonormal states; its length is the dimension of the span of *vecs*. Linearly dependent states are removed.
- *basis* must be orthonormal. Components of *vecs* outside the span of *basis* are discarded: `orthogvc` effectively projects onto the span of *basis* before orthogonalizing.
- Symbolic coefficients are supported; the coefficients are simplified with `Simplify`, which can be slow for complicated expressions.
- `orthogop` is the corresponding function for the creation operator representation. `transformbasis` uses `orthogvc` after applying an operator (e.g. a projector) to the states of each subspace.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
makebasis[{d[]}];
orthogvc[{vc[1, 0] + vc[0, 1], vc[1, 0]}, {vc[1, 0], vc[0, 1]}] // InputForm
```

### Scope
Linearly dependent states are removed:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
orthogvc[{vc[1, 0] + vc[0, 1], vc[1, 0], 2 vc[0, 1]}, {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}] // InputForm
```
---
Symbolic coefficients:
```wl
snegrealconstants[a];
orthogvc[{a vc[1, 0] + vc[0, 1]}, {vc[1, 0], vc[0, 1]}] // InputForm
```

### Applications
Projecting the two-site states with one electron per site: the projector $P_1 = \prod_i (n_{i\uparrow} - n_{i\downarrow})^2$ applied to the half-filled subspace with $S_z = 0$ leaves two states:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
states = Last[First[Select[bz, First[#] == {0, 0} &]]];
P = nc[projector1[c[1]], projector1[c[2]]];
orthogvc[ap[P, states], states] // InputForm
```

### Properties & Relations
The result is orthonormal:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
o = orthogvc[{vc[1, 0] + 2 vc[0, 1] + vc[1, 1], vc[1, 0] - vc[1, 1]}, {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}];
Outer[scalarproductvc, o, o]
```

### Possible Issues
Components outside the span of *basis* are lost:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
orthogvc[{vc[1, 0]}, {vc[0, 1]}]
```
