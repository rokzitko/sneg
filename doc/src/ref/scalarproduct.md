---
keywords: scalar product, inner product, overlap, occupation number representation
see also: scalarproductvc, scalarproductop, normvc
---
# scalarproduct

## Usage
- `scalarproduct[a, b]` gives the scalar product $\langle a | b \rangle$ of states in the occupation number representation; it is an alias of `scalarproductvc`.

## Details
- `scalarproduct[a, b]` evaluates to `scalarproductvc[a, b]`; see that page for the conventions. The first argument is conjugated.
- For states in the creation operator representation, use `scalarproductop`.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
makebasis[{d[]}];
scalarproduct[(vc[1, 0] + vc[0, 1])/Sqrt[2], vc[1, 0]]
```

### Properties & Relations
```wl
snegfermionoperators[d];
makebasis[{d[]}];
scalarproduct[I vc[1, 0], vc[1, 0]] === scalarproductvc[I vc[1, 0], vc[1, 0]]
```
