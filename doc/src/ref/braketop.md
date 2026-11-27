---
keywords: matrix element, braket, creation operator representation, vacuum expectation value
see also: braketvc, expvop, scalarproductop, vev, matrixrepresentationop
---
# braketop

## Usage
- `braketop[a, op, b]` gives the matrix element $\langle a | \hat O | b \rangle$, where the states are given in the creation operator representation, $|a\rangle = \hat A |0\rangle$ and $|b\rangle = \hat B |0\rangle$.

## Details
- `braketop[a, op, b]` computes the vacuum expectation value $\langle 0 | \hat A^\dagger \hat O \hat B | 0 \rangle$ with `vev`. *a* and *b* are operator expressions, usually strings of creation operators; 1 represents the vacuum.
- `braketop` does not require `makebasis`; it works for any operators known to `vev`.
- `braketop` is `Listable`. `expvop[op, a]` is `braketop[a, op, a]`. For states in the occupation number representation, use `braketvc`, which is faster.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
braketop[c[CR, 2, UP], hop[c[1], c[2]], c[CR, 1, UP]]
```
Pair annihilation:
```wl
braketop[1, nc[c[AN, 1, DO], c[AN, 1, UP]], nc[c[CR, 1, UP], c[CR, 1, DO]]]
```

### Scope
Symbolic level indexes, declared with `snegfreeindexes`, give Kronecker deltas:
```wl
snegfermionoperators[c];
snegfreeindexes[i, j, k];
braketop[c[CR, i, UP], hop[c[1], c[2]], c[CR, k, UP]]
```

### Properties & Relations
`braketvc` gives the same matrix elements for the corresponding `vc` states:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
a = nc[c[CR, 1, UP], c[CR, 2, DO]];
b = nc[c[CR, 1, DO], c[CR, 2, UP]];
{braketop[a, spinspin[c[1], c[2]], b], braketvc[ops2vc[a], spinspin[c[1], c[2]], ops2vc[b]]}
```

### Possible Issues
With undeclared symbolic indexes, the Kronecker deltas are treated as operators and the result is not simplified:
```wl
snegfermionoperators[c];
braketop[c[CR, p, UP], number[c[q]], c[CR, r, UP]]
```
```wl
snegfreeindexes[p, q, r];
braketop[c[CR, p, UP], number[c[q]], c[CR, r, UP]]
```
