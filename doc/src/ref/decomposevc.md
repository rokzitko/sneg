---
keywords: decomposition, components, expansion coefficients, projection, occupation number representation
see also: decomposeop, scalarproductvc, orthogvc, vc
---
# decomposevc

## Usage
- `decomposevc[v, basis]` gives the components $\langle b_j | v\rangle$ of the state *v* with respect to the states $b_j$ of *basis*, in the occupation number representation.

## Details
- The result is the list `scalarproductvc[#, v] & /@ basis`. For an orthonormal *basis* spanning *v*, $v = \sum_j \langle b_j | v\rangle\, b_j$, i.e. `v == decomposevc[v, basis] . basis`.
- `decomposeop` is the corresponding function for the creation operator representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
makebasis[{d[]}];
decomposevc[(vc[1, 0] + I vc[0, 1])/Sqrt[2], {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}]
```

### Applications
Components of the two-site singlet in the $(Q, S_z) = (0, 0)$ subspace:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
states = Last[First[Select[bz, First[#] == {0, 0} &]]];
singlet = (vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];
coef = decomposevc[singlet, states]
```
```wl
Expand[coef . states - singlet]
```
