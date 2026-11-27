---
keywords: contraction, Wick's theorem, propagator, normal ordering, Fermi sea, occupation number
see also: wick, vevwick, vev, normalorder, dd, ordering, acmt, cmt
---
# contraction

## Usage
- `contraction[a, b]` gives the contraction of the operators *a* and *b*, $\langle a b \rangle = a b - {:}a b{:}$, in this order.

## Details
- The contraction is the difference between the product and its normal-ordered form with respect to the vacuum set by `ordering`. It is a c-number and equals the vacuum expectation value $\langle 0| a b |0\rangle$.
- For fermions with the empty vacuum (`ordering[c] = EMPTY`, the default), $\langle c_\alpha c^\dagger_\beta \rangle = \delta_{\alpha\beta}$ and all other contractions vanish. For bosons, $\langle a_\alpha a^\dagger_\beta \rangle = \delta_{\alpha\beta}$.
- For the Fermi sea (`ordering[c] = SEA`), $\langle c^\dagger_{k\sigma} c_{k'\sigma'} \rangle = \delta_{kk'}\delta_{\sigma\sigma'}\theta(-k)$ and $\langle c_{k\sigma} c^\dagger_{k'\sigma'} \rangle = \delta_{kk'}\delta_{\sigma\sigma'}(1 - \theta(-k))$, where *k* is the second index (the energy measured from the Fermi level) and $\theta$ is `UnitStep`.
- For operators with `ordering[c] = NONE`, the contraction of an annihilation operator with a creation operator to its right is the canonical (anti)commutator `acmt` or `cmt`, and all other contractions are zero.
- Operators of different families have zero contraction.
- The arguments must be single operators. The order of the arguments matters: `contraction[a, b]` is the contraction with *a* to the left of *b*.

## Developer Notes
- For orderings other than `NONE`, `contraction[a, b]` is computed as `nc[a, b] - normalorder[nc[a, b]]`, i.e., `vev[nc[a, b]]`. Redefining the expectation values of bilinears for a new vacuum therefore defines the contractions used by `wick` and `vevwick`.

## Examples
### Basic Examples
Contractions for the empty vacuum:
```wl
snegfermionoperators[c];
snegfreeindexes[i, j];
{contraction[c[AN, i, UP], c[CR, j, UP]], contraction[c[CR, i, UP], c[AN, j, UP]]}
```
---
Contractions for the Fermi sea, $\langle c^\dagger_k c_{k'} \rangle = \delta_{kk'} n_k$ and $\langle c_k c^\dagger_{k'} \rangle = \delta_{kk'} (1 - n_k)$:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k1, k2];
{contraction[f[CR, k1, UP], f[AN, k2, UP]], contraction[f[AN, k1, UP], f[CR, k2, UP]]}
```

### Scope
Symbolic spin indexes give Kronecker deltas:
```wl
snegfermionoperators[c];
snegfreeindexes[i, j, s1, s2];
contraction[c[AN, i, s1], c[CR, j, s2]]
```
---
Two creation operators or two annihilation operators have zero contraction:
```wl
snegfermionoperators[c];
{contraction[c[CR, 1, UP], c[CR, 2, UP]], contraction[c[AN, 1, UP], c[AN, 2, UP]]}
```
---
Bosons:
```wl
snegbosonoperators[a];
{contraction[a[AN], a[CR]], contraction[a[CR], a[AN]]}
```
---
Explicit levels below and above the Fermi level:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
{contraction[f[CR, -2, UP], f[AN, -2, UP]], contraction[f[CR, 3, UP], f[AN, 3, UP]], contraction[f[AN, 3, UP], f[CR, 3, UP]]}
```
---
Operators with `ordering[g] = NONE`:
```wl
snegfermionoperators[g];
ordering[g] = NONE;
{contraction[g[AN, 1, UP], g[CR, 1, UP]], contraction[g[CR, 1, UP], g[AN, 1, UP]]}
```

### Properties & Relations
The contraction is the vacuum expectation value of the product:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k1, k2];
contraction[f[AN, k1, UP], f[CR, k2, UP]] == vev[nc[f[AN, k1, UP], f[CR, k2, UP]]]
```
---
The sum of the contractions in both orders is the anticommutator:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k1, k2];
Simplify[contraction[f[AN, k1, UP], f[CR, k2, UP]] + contraction[f[CR, k2, UP], f[AN, k1, UP]]]
```
---
`wick` expresses products in terms of contractions and normal-ordered products:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k1, k2];
wick[nc[f[CR, k1, UP], f[AN, k2, UP]]]
```

### Possible Issues
The arguments must be operators. Products and undeclared symbols are not contracted:
```wl
snegfermionoperators[c];
contraction[u, c[CR, 1, UP]]
```
