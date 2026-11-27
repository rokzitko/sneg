---
keywords: anticommutator, canonical anticommutation relations, CAR, Dirac delta, non-orthogonal orbitals, field operators, extending SNEG
see also: cmt, snegOrderedQ, ordering, antikomutator, nc, snegfermionoperators, snegmajoranaoperators
---
# acmt

## Usage
- `acmt[op1, op2]` gives the anticommutator $\{op_1, op_2\}$ of two fermion or Majorana operators *op1* and *op2*, which `nc` uses when it exchanges them.

## Details
- When `nc` exchanges two adjacent operators of the same fermion family that are not in canonical order (see `snegOrderedQ`), it uses $x_1 x_2 = -x_2 x_1 + \{x_1, x_2\}$, with the anticommutator given by `acmt[x1, x2]`. `acmt` thus defines the algebra of the operators; `antikomutator`, `komutator`, `vevwick`, and all other functions follow from it.
- The built-in values are the canonical anticommutation relations:

| operators | `acmt` |
|---|---|
| `c[t1, j1, ...]`, `c[t2, j2, ...]` with `t1` ≠ `t2` (one `CR`, one `AN`) | $\prod_n \delta_{j_n j'_n}$ (`KroneckerDelta` of the corresponding indexes); 0 if the numbers of indexes differ |
| `c[t, ...]`, `c[t, ...]` (same type) | 0 |
| `c[CR]`, `c[AN]` (no further indexes) | 1 |
| operators of different fermion families | 0 |
| Majorana operators `g[j1, ...]`, `g[j2, ...]` | $\prod_n \delta_{j_n j'_n}$ |
| Majorana operators of different families | 0 |
| bare Majorana symbols `g`, `g` | 1 |

- The type indexes are compared literally: two operators with different first indexes count as one creation and one annihilation operator.
- Since the Kronecker deltas are written for symbolic indexes, the relations hold for arbitrary index values, e.g. $\{c_{k\sigma}, c^\dagger_{k'\sigma'}\} = \delta_{kk'}\delta_{\sigma\sigma'}$.
- For undeclared symbols `acmt` remains unevaluated.

## Developer Notes
- Other anticommutation relations are defined by new definitions of `acmt` for a fermion family, preferably as upvalues of the operator symbol, `c /: acmt[c[AN, ...], c[CR, ...]] := ...`, which take precedence over the built-in rules. With the default `ordering` (EMPTY), `nc` only calls `acmt[c[AN, ...], c[CR, ...]]`; with other orderings also `acmt[c[CR, ...], c[AN, ...]]` occurs, so both should be defined.
- The value must be a scalar for `nc` to factor it out. Functions such as `DiracDelta` are scalars only if declared, e.g. with `snegnonopQ[DiracDelta] = True` (see `isnumericQ`).
- The anticommutator of operators of different families is always zero in `nc`; it is not looked up in `acmt`.
- For bosons and spin operators, the commutator is defined by `cmt`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{acmt[c[AN, 1, UP], c[CR, 1, UP]], acmt[c[AN, 1, UP], c[CR, 2, UP]], acmt[c[CR, 1, UP], c[CR, 1, UP]]}
```
`nc` uses the anticommutator when it reorders a product:
```wl
nc[c[AN, 1, UP], c[CR, 1, UP]]
```

### Scope
Symbolic indexes give Kronecker deltas:
```wl
snegfermionoperators[c];
{acmt[c[AN, i, s], c[CR, j, t]], nc[c[AN, i, s], c[CR, j, t]]}
```
---
Operators with different numbers of indexes, and operators of different families, anticommute:
```wl
snegfermionoperators[c, d];
{acmt[c[AN, 1, UP], c[CR, UP]], acmt[c[AN, 1, UP], d[CR, 1, UP]]}
```
---
Majorana operators, $\{\gamma_i, \gamma_j\} = \delta_{ij}$:
```wl
snegmajoranaoperators[g];
{acmt[g[1], g[1]], acmt[g[1], g[2]], acmt[g, g]}
```

### Applications
Field operators of a continuum, $\{\psi(x), \psi^\dagger(y)\} = \delta(x - y)$. The anticommutator is redefined as an upvalue of `psi`, and `DiracDelta` is declared as a scalar:
```wl
snegspinlessfermionoperators[psi];
snegrealconstants[x, y, z];
snegnonopQ[DiracDelta] = True;
psi /: acmt[psi[AN, x_], psi[CR, y_]] := DiracDelta[x - y];
psi /: acmt[psi[CR, x_], psi[AN, y_]] := DiracDelta[x - y];
nc[psi[AN, x], psi[CR, y], psi[CR, z]]
```
The two-particle vacuum amplitude $\langle 0|\psi(x)\psi(z)\psi^\dagger(z)\psi^\dagger(y)|0\rangle$:
```wl
vevwick[nc[psi[AN, x], psi[AN, z], psi[CR, z], psi[CR, y]]]
```
---
Non-orthogonal orbitals for the operators `h`, with the overlap $S = \langle 1|2\rangle$, $\{c_{i\sigma}, c^\dagger_{j\sigma'}\} = \delta_{\sigma\sigma'} S_{ij}$ with $S_{ii} = 1$, $S_{12} = S_{21} = S$:
```wl
snegfermionoperators[h];
snegrealconstants[S];
h /: acmt[h[AN, i_, s1_], h[CR, j_, s2_]] := KroneckerDelta[s1, s2] If[i === j, 1, S];
nc[h[AN, 1, UP], h[CR, 2, UP]]
```
The norm of the Heitler-London singlet $(c^\dagger_{1\uparrow} c^\dagger_{2\downarrow} - c^\dagger_{1\downarrow} c^\dagger_{2\uparrow})|0\rangle$ of the hydrogen molecule is $2(1 + S^2)$:
```wl
psiHL = nc[h[CR, 1, UP], h[CR, 2, DO]] - nc[h[CR, 1, DO], h[CR, 2, UP]];
Simplify[vev[nc[conj[psiHL], psiHL]]]
```

### Properties & Relations
`antikomutator` computed with `nc` agrees with `acmt`:
```wl
snegfermionoperators[c];
{antikomutator[c[AN, k, UP], c[CR, q, UP]], acmt[c[AN, k, UP], c[CR, q, UP]]}
```
---
For bosons, the commutator is given by `cmt`:
```wl
snegbosonoperators[a];
{cmt[a[AN, 1], a[CR, 1]], acmt[a[AN, 1], a[CR, 1]]}
```

### Possible Issues
Functions that are not declared as scalars are not factored out of products. Here the undeclared function `w` is treated as an operator:
```wl
snegspinlessfermionoperators[phi];
phi /: acmt[phi[AN, x_], phi[CR, y_]] := w[x - y];
nc[phi[AN, 1], phi[CR, 2], phi[CR, 3]]
```
After the declaration of `w` as a real function, it is factored out:
```wl
snegrealfunctions[w];
nc[phi[AN, 1], phi[CR, 2], phi[CR, 3]]
```
