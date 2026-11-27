---
keywords: Grassmann number, Grassmann variable, anticommuting c-number, coherent state, path integral, Berezin integral
see also: grassmanQ, int, conj, snegfermionoperators, listgrassmanconstants
---
# sneggrassmanconstants

## Usage
- `sneggrassmanconstants[x, y, ...]` declares *x*, *y*, ... as Grassmann numbers (anticommuting constants).

## Details
- Grassmann numbers anticommute with each other and with fermion and Majorana operators, $\xi\eta = -\eta\xi$, $\xi c = -c\,\xi$, and $\xi^2 = 0$. They commute with bosons and spin operators.
- In products, Grassmann numbers are moved to the left of all operators (with the sign of the permutation) and sorted with `OrderedQ`.
- The conjugate `conj[x]` is an independent Grassmann number $\bar\xi$, shown as $\bar x$. `conj` of products reverses the order: $\overline{\xi\eta} = \bar\eta\bar\xi$.
- Grassmann numbers are not scalars in the sense of `isnumericQ`: they are kept inside `nc[...]` products.
- `int[x]` denotes the Berezin integral $\int d\xi$; see `int`.
- The declaration sets the upvalues `grassmanQ[x] = True` and `grassmanQ[conj[x]] = True` and adds *x* to `listgrassmanconstants`.

## Examples
### Basic Examples
```wl
sneggrassmanconstants[x, y];
{nc[y, x], nc[x, x], conj[x]}
```

### Scope
Grassmann numbers anticommute with fermion operators and commute with bosons:
```wl
sneggrassmanconstants[x];
snegfermionoperators[c];
snegbosonoperators[a];
{nc[c[CR, 1, UP], x], nc[a[CR], x]}
```
---
Conjugation of a product:
```wl
sneggrassmanconstants[x, y];
conj[nc[x, y]]
```

### Applications
Fermion coherent state $|\xi\rangle = e^{-\xi c^\dagger}|0\rangle = (1 - \xi c^\dagger)|0\rangle$ is an eigenstate of the annihilation operator, $c|\xi\rangle = \xi|\xi\rangle$:
```wl
sneggrassmanconstants[x];
snegfermionoperators[c];
coh = nc[1 - nc[x, c[CR, UP]], VACUUM];
nc[c[AN, UP], coh] - nc[x, coh]
```

### Properties & Relations
Grassmann numbers are not scalars, but products of two of them commute with operators:
```wl
sneggrassmanconstants[x, y];
snegfermionoperators[c];
{isnumericQ[x], nc[c[CR, 1, UP], x, y] - nc[x, y, c[CR, 1, UP]]}
```
