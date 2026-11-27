---
keywords: matrix element, braket, transition amplitude, occupation number representation
see also: braket, expvvc, braketop, scalarproductvc, ap, matrixrepresentationvc, makeallmatricesbzvc
---
# braketvc

## Usage
- `braketvc[a, op, b]` gives the matrix element $\langle a | \hat O | b \rangle$ of the operator *op* between the states *a* and *b* in the occupation number representation.

## Details
- `braketvc[a, op, b]` is `scalarproductvc[a, ap[op, b]]`. The bra state *a* is conjugated.
- `braketvc` is `Listable` in all arguments. `braket` is an alias.
- The matrices of an operator in a list of states are obtained with `matrixrepresentationvc`, and in all pairs of invariant subspaces of a basis with `makeallmatricesbzvc`.
- For states in the creation operator representation, use `braketop`.

## Examples
### Basic Examples
Hopping amplitude between two one-electron states:
```wl
snegfermionoperators[c];
snegrealconstants[t];
makebasis[{c[1], c[2]}];
braketvc[vc[0, 0, 1, 0], -t hop[c[1], c[2]], vc[1, 0, 0, 0]]
```

### Scope
Matrix elements of a creation operator between the vacuum and the singly occupied states:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
braketvc[{vc[1, 0], vc[0, 1]}, d[CR, UP], vacuum[]]
```

### Applications
Matrix element of the spin-flip operator $S^+_1 S^-_2$ between $|\downarrow,\uparrow\rangle$ and $|\uparrow,\downarrow\rangle$, which enters the exchange coupling:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
braketvc[vc[1, 0, 0, 1], nc[spinplus[c[1]], spinminus[c[2]]], vc[0, 1, 1, 0]]
```
---
Spectral weight of the electron addition to the empty Hubbard atom, $|\langle \uparrow | d^\dagger_\uparrow | 0\rangle|^2$, and of the addition of a spin-down electron to the spin-up state:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
Abs[{braketvc[vc[1, 0], d[CR, UP], vc[0, 0]], braketvc[vc[1, 1], d[CR, DO], vc[1, 0]]}]^2
```

### Properties & Relations
Hermitian conjugation: $\langle a | \hat O | b\rangle^* = \langle b | \hat O^\dagger | a\rangle$:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
op = I nc[c[CR, 2, UP], c[AN, 1, UP]];
a = vc[0, 0, 1, 0]; b = vc[1, 0, 0, 0];
Conjugate[braketvc[a, op, b]] == braketvc[b, conj[op], a]
```
