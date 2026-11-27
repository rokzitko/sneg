---
keywords: scalar product, inner product, overlap, braket, creation operator representation, state
see also: normop, expvop, orthogop, decomposeop, braketop, matrixrepresentationop, scalarproductvc, vevwick, zeroonvac
---
# scalarproductop

## Usage
- `scalarproductop[a, b]` gives the scalar product $\langle a | b \rangle$ of the states $|a\rangle = a|0\rangle$ and $|b\rangle = b|0\rangle$ given in the creation-operator representation.

## Details
- In the creation-operator representation, a state is an operator expression acting on the vacuum, e.g. `nc[c[CR, 1, UP], c[CR, 2, DO]]` for $c^\dagger_{1\uparrow} c^\dagger_{2\downarrow}|0\rangle$.
- The result is $\langle 0| a^\dagger b |0\rangle$, computed as `vevwick[nc[conj[a], zeroonvac[b]]]`. The first argument is conjugated: `scalarproductop` is antilinear in *a* and linear in *b*.
- The vacuum is the one set by `ordering`: the empty state or, for `ordering[c] = SEA`, the Fermi sea.
- Terms of *b* that annihilate the vacuum are dropped by `zeroonvac`.
- `scalarproductop` is `Listable`.

## Examples
### Basic Examples
Overlaps of two-electron states on two sites. The sign depends on the order of the creation operators:
```wl
snegfermionoperators[c];
s = nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]];
t0 = nc[c[CR, 1, UP], c[CR, 2, DO]] + nc[c[CR, 1, DO], c[CR, 2, UP]];
{scalarproductop[s, s], scalarproductop[s, t0], scalarproductop[t0, nc[c[CR, 2, DO], c[CR, 1, UP]]]}
```

### Scope
The first argument is complex conjugated:
```wl
snegfermionoperators[c];
snegcomplexconstants[z];
{scalarproductop[z c[CR, 1, UP], c[CR, 1, UP]], scalarproductop[c[CR, 1, UP], z c[CR, 1, UP]]}
```
---
States in the Fermi sea: a particle-hole excitation is normalized, and adding an electron to an occupied level gives zero:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
{scalarproductop[nc[f[CR, 1, UP], f[AN, -1, UP]], nc[f[CR, 1, UP], f[AN, -1, UP]]], scalarproductop[f[CR, -1, UP], f[CR, -1, UP]]}
```

### Applications
Matrix of the Hubbard dimer in the basis of states with one spin-up and one spin-down electron:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
b = {nc[c[CR, 1, UP], c[CR, 2, DO]], nc[c[CR, 1, DO], c[CR, 2, UP]], nc[c[CR, 1, UP], c[CR, 1, DO]], nc[c[CR, 2, UP], c[CR, 2, DO]]};
Outer[scalarproductop[#1, nc[H, #2]] &, b, b] // MatrixForm
```

### Properties & Relations
`normop` is the square root of the scalar product of a state with itself:
```wl
snegfermionoperators[c];
s = nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]];
{Sqrt[scalarproductop[s, s]], normop[s]}
```
---
`matrixrepresentationop` computes the matrix elements with `scalarproductop`:
```wl
snegfermionoperators[c];
b = {c[CR, 1, UP], c[CR, 2, UP]};
matrixrepresentationop[hop[c[1], c[2]], b]
```

### Possible Issues
Operator expressions that end with an annihilation operator are not states; they are dropped:
```wl
snegfermionoperators[c];
scalarproductop[c[CR, 1, UP], c[CR, 1, UP] + nc[c[CR, 1, UP], c[AN, 2, UP]]]
```
