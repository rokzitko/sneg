---
keywords: normal ordering, normal order, vacuum expectation value, Fermi sea, particle-hole, zero-point energy
see also: normalorderwick, vev, vevwick, wick, dd, ordering, contraction
---
# normalorder

## Usage
- `normalorder[expr]` gives `expr - vev[expr]`, the operator *expr* with its vacuum expectation value subtracted.

## Details
- For a product of two operators, `normalorder` gives the normal-ordered product with respect to the vacuum set by `ordering`, ${:}A B{:} = A B - \langle A B \rangle$. For the Fermi sea (`ordering[c] = SEA`), ${:}c^\dagger_k c_k{:} = c^\dagger_k c_k - \theta(-k)$.
- For longer products, $x - \langle x \rangle$ is not the normal-ordered product: Wick's theorem also subtracts the partial contractions. Use `wick` to obtain ${:}x{:}$ for such products.
- The vacuum expectation value is computed by `vev`. For the Fermi sea, `vev` does not evaluate products of more than two operators with symbolic indexes; `normalorderwick` uses `vevwick` instead.
- `normalorder` is `Listable`.
- For the empty vacuum, `nc` already brings products to normal order; `normalorder` then subtracts the constant term.

## Examples
### Basic Examples
Normal ordering with respect to the Fermi sea, ${:}c^\dagger_k c_k{:} = c^\dagger_k c_k - \theta(-k)$:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k];
normalorder[nc[c[CR, k, UP], c[AN, k, UP]]]
```
Its expectation value vanishes:
```wl
vev[%]
```

### Scope
Products of an annihilation and a creation operator:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2];
normalorder[nc[c[AN, k1, UP], c[CR, k2, UP]]]
```
---
For the empty vacuum, the constant term of the normal-ordered product is subtracted:
```wl
snegfermionoperators[d];
normalorder[nc[d[AN, 1, UP], d[CR, 1, UP]]]
```
---
Bosons, ${:}a a^\dagger{:} = a^\dagger a$:
```wl
snegbosonoperators[a];
normalorder[nc[a[AN], a[CR]]]
```
---
`normalorder` threads over lists:
```wl
snegbosonoperators[a];
normalorder[{nc[a[AN], a[CR]], nc[a[CR], a[AN]]}]
```

### Applications
Harmonic oscillator $H = \omega (p^2 + x^2)/2$ with $x = (a + a^\dagger)/\sqrt{2}$, $p = i(a^\dagger - a)/\sqrt{2}$. Normal ordering removes the zero-point energy $\omega/2$:
```wl
snegbosonoperators[a];
snegrealconstants[ω];
x = (a[AN] + a[CR])/Sqrt[2];
p = I (a[CR] - a[AN])/Sqrt[2];
H = Expand[ω (pow[p, 2] + pow[x, 2])/2]
```
```wl
normalorder[H]
```
---
The band Hamiltonian measured from the energy of the filled Fermi sea, $H_0 - E_\mathrm{FS} = \sum_k \epsilon_k {:}c^\dagger_k c_k{:}$:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k];
snegrealfunctions[eps];
normalorder[sum[eps[k] nc[c[CR, k, UP], c[AN, k, UP]], {k}]]
```

### Properties & Relations
`normalorder[x]` is `x - vev[x]`:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2];
x = nc[c[CR, k1, UP], c[AN, k2, UP]];
normalorder[x] == x - vev[x]
```
---
For bilinears, `normalorder` agrees with `normalorderwick`:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2];
x = nc[c[AN, k1, UP], c[CR, k2, UP]];
normalorder[x] - normalorderwick[x]
```

### Possible Issues
For products of four operators in the Fermi sea, `vev` is not fully evaluated, and the result contains `vev[...]`:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2, k3, k4];
x = nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP], c[AN, k4, UP]];
normalorder[x]
```
---
For products of more than two operators, `x - vev[x]` is not the normal-ordered product ${:}x{:}$. Here ${:}a a a^\dagger a^\dagger{:} = a^{\dagger 2} a^2$ is the last term of the expansion by `wick`, while `normalorder` also keeps the term $4 a^\dagger a$ with one contraction:
```wl
snegbosonoperators[a];
x = nc[a[AN], a[AN], a[CR], a[CR]];
{normalorder[x], wick[x]}
```
