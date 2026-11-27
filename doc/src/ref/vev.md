---
keywords: vacuum expectation value, expectation value, ground state, Fermi sea, normal ordering, VACUUM, Wick's theorem
see also: vevwick, normalorder, ordering, wick, contraction, scalarproductop, expvop, VACUUM, zeroonvac
---
# vev

## Usage
- `vev[expr]` gives the vacuum expectation value $\langle 0 | \mathit{expr} | 0 \rangle$ of the operator expression *expr*.

## Details
- The vacuum $|0\rangle$ is the reference state of the operator family, set by `ordering`: the empty state (no particles) for `ordering[c] = EMPTY` (the default) and the Fermi sea for `ordering[c] = SEA`. For bosons, the vacuum is the state without bosons.
- `vev` is linear: it distributes over sums, and numbers, declared constants, and Grassmann numbers are taken out. `vev[x] = x` for scalars *x*. `vev[sum[a, {k}]]` is `sum[vev[a], {k}]`.
- A product `nc[o1, ..., on]` has zero expectation value if its last factor annihilates the vacuum (`isannihilation`) or its first factor annihilates the bra $\langle 0|$ (`iscreation`).
- A single fermion or boson operator and a product of an odd number of fermion or boson operators have zero expectation value: the vacuum has a definite particle number.
- For `ordering[c] = SEA`, the bilinears are $\langle c^\dagger_{k\sigma} c_{k'\sigma'} \rangle = \delta_{kk'}\delta_{\sigma\sigma'}\,\theta(-k)$ and $\langle c_{k\sigma} c^\dagger_{k'\sigma'} \rangle = \delta_{kk'}\delta_{\sigma\sigma'}\,\theta(k)$, where $\theta$ is `UnitStep` with $\theta(0) = 1$.
- `vev[dd[...]] = 0`: the expectation value of a normal-ordered product (see `dd`) vanishes by definition. `vev[vev[x]] = vev[x]`.
- For `ordering[c] = SEA`, the second index of `c[t, k, ..., σ]` (the first after `CR`/`AN`) is the single-particle energy measured from the Fermi level: levels with $k \le 0$ are occupied in the Fermi sea, levels with $k > 0$ are empty. `iscreation` and `isannihilation` evaluate the sign of *k*; for symbolic *k* they remain undecided and the corresponding rules do not apply.
- For `ordering[c] = EMPTY`, `nc` brings every product to normal order, so `vev` only has to pick the terms without operators. For the Fermi sea, products of more than two operators with symbolic indexes are generally not evaluated by `vev`; use `vevwick`, which applies Wick's theorem.
- With the state placeholder `VACUUM`, `nc[conj[VACUUM], VACUUM]` is 1, so matrix elements can also be written out explicitly.
- `vev` is `Listable`. Expressions with undeclared symbols (treated as operators of unknown type) are returned unevaluated.

## Developer Notes
- `vev` is declared with `sneglinearoperatorFirst`. The Fermi-sea bilinears are evaluated by the rule for `nc[op1[i1, k1, j1...], op2[i2, k2, j2...]]`, which is also used by `contraction` and hence by `vevwick` and `wick`.
- The rules for `SEA` include the selection rule that a product with nonzero total charge or spin projection has zero expectation value (operators with explicit `UP`/`DO` spin indexes).

## Examples
### Basic Examples
Expectation values in the empty vacuum, $\langle 0| c_{1\uparrow} c^\dagger_{1\uparrow} |0\rangle = 1$ and $\langle 0| c^\dagger_{1\uparrow} c_{1\uparrow} |0\rangle = 0$:
```wl
snegfermionoperators[c];
{vev[nc[c[AN, 1, UP], c[CR, 1, UP]]], vev[nc[c[CR, 1, UP], c[AN, 1, UP]]]}
```
---
Expectation values in the Fermi sea, $\langle n_{k\uparrow} \rangle = \theta(-k)$:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k, k1, k2];
{vev[nc[f[CR, k, UP], f[AN, k, UP]]], vev[nc[f[AN, k, UP], f[CR, k, UP]]]}
```
```wl
vev[nc[f[CR, k1, UP], f[AN, k2, UP]]]
```

### Scope
`vev` is linear and takes constants out:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
vev[U + t nc[c[AN, 1, UP], c[CR, 1, UP]] + t nc[c[AN, 2, DO], c[CR, 2, DO]]]
```
---
Products of four operators in the empty vacuum. The product is normal ordered by `nc`, and `vev` picks the constant term:
```wl
snegfermionoperators[c];
vev[nc[c[AN, 1, UP], c[AN, 2, UP], c[CR, 2, UP], c[CR, 1, UP]]]
```
---
Boson operators, $\langle 0| a^2 (a^\dagger)^2 |0\rangle = 2$:
```wl
snegbosonoperators[a];
vev[nc[a[AN], a[AN], a[CR], a[CR]]]
```
---
Sums are mapped over. The band energy of the filled Fermi sea, $\sum_{k\sigma} \epsilon_k \theta(-k)$:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k, σ];
snegrealfunctions[eps];
vev[sum[eps[k] nc[f[CR, k, σ], f[AN, k, σ]], {k, σ}]]
```
---
Explicit indexes decide whether a level is occupied:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
{vev[nc[f[CR, -1, UP], f[AN, -1, UP]]], vev[nc[f[CR, 2, UP], f[AN, 2, UP]]], vev[nc[f[AN, -1, UP], f[CR, -1, UP]]]}
```
---
`vev` threads over lists:
```wl
snegfermionoperators[c];
vev[{number[c[1]], nc[c[AN, 1, UP], c[CR, 1, UP]]}]
```

### Applications
Matrix element of the Hubbard Hamiltonian between two-electron states, $\langle \psi | H | \psi \rangle$ with the singlet $|\psi\rangle = (c^\dagger_{1\uparrow} c^\dagger_{2\downarrow} - c^\dagger_{1\downarrow} c^\dagger_{2\uparrow})|0\rangle/\sqrt2$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
psi = (nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];
vev[nc[conj[psi], H, psi]]
```
The doubly occupied states are connected to the singlet by hopping:
```wl
d = (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[CR, 2, UP], c[CR, 2, DO]])/Sqrt[2];
vev[nc[conj[d], H, psi]]
```
---
Kinetic energy of the filled Fermi sea for a band $\epsilon_k$ with the Fermi level at zero energy, written with explicit levels $k = -2, \ldots, 2$:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
H = Sum[k number[f[k]], {k, -2, 2}];
vev[H]
```

### Properties & Relations
`vev` of a normal-ordered expression vanishes:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k1, k2];
vev[normalorder[nc[f[CR, k1, UP], f[AN, k2, UP]]]]
```
---
`vev` agrees with `vevwick` on products of two operators:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k1, k2];
x = nc[f[AN, k1, UP], f[CR, k2, UP]];
{vev[x], vevwick[x]}
```
---
With the `VACUUM` placeholder, the vacuum expectation value is $\langle 0| \cdots |0\rangle$ written out:
```wl
snegfermionoperators[c];
{nc[conj[VACUUM], c[AN, 1, UP], c[CR, 1, UP], VACUUM], vev[nc[c[AN, 1, UP], c[CR, 1, UP]]]}
```

### Possible Issues
For the Fermi sea, longer products with symbolic indexes are not evaluated by `vev`. Use `vevwick`:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k1, k2, k3, k4];
x = nc[f[CR, k1, UP], f[AN, k2, UP], f[CR, k3, UP], f[AN, k4, UP]];
vev[x]
```
```wl
vevwick[x]
```
---
Products of boson operators that start with creation operators are not reduced to zero unless they end with an annihilation operator. `vevwick` gives the result:
```wl
snegbosonoperators[a];
{vev[nc[a[CR], a[CR]]], vevwick[nc[a[CR], a[CR]]]}
```
---
Undeclared symbols are treated as operators of unknown type; the expectation value is not evaluated:
```wl
snegfermionoperators[c];
vev[nc[c[AN, 1, UP], u, c[CR, 1, UP]]]
```
```wl
snegrealconstants[u];
vev[nc[c[AN, 1, UP], u, c[CR, 1, UP]]]
```
