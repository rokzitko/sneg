---
keywords: norm, normalization, state, creation operator representation
see also: scalarproductop, expvop, orthogop, normvc
---
# normop

## Usage
- `normop[a]` gives the norm $\sqrt{\langle a | a \rangle}$ of the state $|a\rangle = a|0\rangle$ in the creation-operator representation.

## Details
- `normop[a]` is the square root of `scalarproductop[a, a]`. The vacuum is the one set by `ordering`.
- `normop` is `Listable`.

## Examples
### Basic Examples
Normalization of the singlet state of two electrons:
```wl
snegfermionoperators[c];
s = nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]];
normop[s]
```
```wl
s/normop[s]
```

### Scope
Complex coefficients:
```wl
snegfermionoperators[c];
snegcomplexconstants[z];
normop[z c[CR, 1, UP]]
```
---
Two electrons in the same spin-orbital: the Pauli principle gives zero norm:
```wl
snegfermionoperators[c];
normop[{nc[c[CR, 1, UP], c[CR, 1, UP]], nc[c[CR, 1, UP], c[CR, 1, DO]]}]
```
---
Bosons, $\| (a^\dagger)^n |0\rangle \| = \sqrt{n!}$:
```wl
snegbosonoperators[a];
Table[normop[pow[a[CR], n]], {n, 0, 4}]
```
