---
keywords: matrix element, braket, occupation number representation
see also: braketvc, braketop, expvvc
---
# braket

## Usage
- `braket[a, op, b]` gives the matrix element $\langle a | \hat O | b \rangle$ in the occupation number representation; it is an alias of `braketvc`.

## Details
- `braket[a, op, b]` evaluates to `braketvc[a, op, b]`, i.e. `scalarproductvc[a, ap[op, b]]`.
- `braket` is `Listable` in all arguments.
- `examples/diagonalize.m` uses `braket` for sums of matrix elements over the eigenstates.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
braket[vc[0, 0, 1, 0], hop[c[1], c[2]], vc[1, 0, 0, 0]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
braket[vc[1, 0, 0, 1], spinspin[c[1], c[2]], vc[0, 1, 1, 0]] === braketvc[vc[1, 0, 0, 1], spinspin[c[1], c[2]], vc[0, 1, 1, 0]]
```
