---
keywords: decomposition, components, expansion coefficients, creation operator representation
see also: decomposevc, scalarproductop, orthogop
---
# decomposeop

## Usage
- `decomposeop[v, basis]` gives the components $\langle b_j | v\rangle$ of the state *v* with respect to the states $b_j$ of *basis*, all in the creation operator representation.

## Details
- The result is the list `scalarproductop[#, v] & /@ basis`, computed with vacuum expectation values. See `decomposevc` for the occupation number representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
decomposeop[(d[CR, UP] + I d[CR, DO])/Sqrt[2], basis[d[]]]
```

### Properties & Relations
The components reconstruct the state:
```wl
snegfermionoperators[d];
v = 2 + nc[d[CR, UP], d[CR, DO]];
b = basis[d[]];
Expand[decomposeop[v, b] . b - v]
```
