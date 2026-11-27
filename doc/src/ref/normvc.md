---
keywords: norm, normalization, state vector, occupation number representation
see also: scalarproductvc, normop, orthogvc, vc
---
# normvc

## Usage
- `normvc[v]` gives the norm $\sqrt{\langle v | v\rangle}$ of the state *v* in the occupation number representation.

## Details
- `normvc[v]` is `Sqrt[vev[scalarproductvc[v, v]]]`.
- `normvc` is `Listable`. The corresponding function for states in the creation operator representation is `normop`.
- Normalized states are obtained as `v/normvc[v]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
makebasis[{d[]}];
normvc[3 vc[1, 0] + 4 I vc[0, 1]]
```

### Scope
Symbolic coefficients:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
snegrealconstants[a, b];
normvc[a vc[0, 0] + b vc[1, 1]]
```
---
`normvc` is `Listable`:
```wl
normvc[{vc[1, 0], vc[1, 0] + vc[0, 1], 0}]
```

### Applications
Normalization of the state obtained by applying the spin-lowering operator $S^-$ to $|\uparrow\uparrow\rangle$:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
v = ap[spinminus[c[1]] + spinminus[c[2]], vc[1, 0, 1, 0]];
v/normvc[v] // InputForm
```

### Properties & Relations
`normop` gives the same norm in the creation operator representation:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
v = vc[1, 0] - 2 vc[0, 1];
{normvc[v], normop[vc2ops[v]]}
```
