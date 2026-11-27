---
keywords: vacuum, annihilation, state, creation operator representation, Fermi sea
see also: VACUUM, scalarproductop, vev, ordering, isannihilation, snegAssuming
---
# zeroonvac

## Usage
- `zeroonvac[expr]` drops the terms of the operator expression *expr* that annihilate the vacuum, i.e., the terms whose rightmost operator is an annihilation operator.

## Details
- `zeroonvac` simplifies states in the creation-operator representation, $|\psi\rangle = \mathit{expr}\,|0\rangle$, where the vacuum $|0\rangle$ is not written. It is used, e.g., after applying an operator to a state: `zeroonvac[nc[op, state]]`.
- A term is dropped if its last operator annihilates the vacuum, as decided by `isannihilation`; the vacuum is the one set by `ordering`. For `ordering[c] = SEA`, $c_{k}$ with $k > 0$ and $c^\dagger_{k}$ with $k < 0$ annihilate the Fermi sea.
- The result relies on the normal order of the products, which `nc` establishes automatically.
- `zeroonvac` is linear and `Listable`. If the term ends with a `ket`, the operators to the left of the ket are examined.
- If the sign of a symbolic index is undecided for `ordering[c] = SEA`, the result contains an unevaluated `If[...]`. `snegAssuming` can be used to specify the sign.
- Symbolic sums `sum[a, {k}]` are not processed; the variant `Zeroonvac` also applies `zeroonvac` to the summands.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
zeroonvac[c[CR, 1, UP] + nc[c[CR, 1, UP], c[AN, 2, UP]] + c[AN, 1, UP]]
```

### Scope
Applying the total spin-lowering operator to $c^\dagger_{1\uparrow} c^\dagger_{2\uparrow}|0\rangle$. The product contains terms with annihilation operators on the right, which vanish on the vacuum:
```wl
snegfermionoperators[c];
v = nc[spinminus[c[1]] + spinminus[c[2]], c[CR, 1, UP], c[CR, 2, UP]]
```
```wl
zeroonvac[v]
```
---
States in the Fermi sea:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
zeroonvac[{nc[f[CR, 2, UP], f[AN, -1, UP]], nc[f[CR, 2, UP], f[AN, 1, UP]], f[CR, -1, UP]}]
```
---
Boson operators:
```wl
snegbosonoperators[a];
zeroonvac[{nc[a[CR], a[AN]], nc[a[CR], a[CR]]}]
```

### Applications
The Hubbard Hamiltonian acting on the state $c^\dagger_{1\uparrow} c^\dagger_{2\downarrow}|0\rangle$ gives the doubly occupied states:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
zeroonvac[nc[H, c[CR, 1, UP], c[CR, 2, DO]]]
```

### Properties & Relations
With the explicit vacuum state `VACUUM`, the annihilation operators acting on the vacuum are dropped by `nc`:
```wl
snegfermionoperators[c];
nc[c[CR, 1, UP] + nc[c[CR, 1, UP], c[AN, 2, UP]], VACUUM]
```

### Possible Issues
For the Fermi sea, the sign of a symbolic index decides whether the operator annihilates the vacuum:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k];
zeroonvac[nc[f[CR, 2, UP], f[AN, k, UP]]]
```
```wl
snegAssuming[k /: k > 0 = True, zeroonvac[nc[f[CR, 2, UP], f[AN, k, UP]]]]
```
---
Symbolic sums are not processed; use `Zeroonvac`:
```wl
snegfermionoperators[c];
snegfreeindexes[q];
x = sum[c[AN, q, UP], {q}] + c[CR, 2, UP];
{zeroonvac[x], Zeroonvac[x]}
```
