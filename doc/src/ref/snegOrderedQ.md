---
keywords: canonical order, normal order, operator ordering, anti-normal ordering, reordering rule, extending SNEG
see also: ordering, acmt, cmt, iscreation, isannihilation, nc, OrderedQ
---
# snegOrderedQ

## Usage
- `snegOrderedQ[op1, op2]` gives `True` if the operators *op1* and *op2* are in the canonical order, i.e. if the product `nc[op1, op2]` is left as it is, and `False` if `nc` exchanges them.

## Details
- `nc` calls `snegOrderedQ` for every pair of adjacent operators. If two operators of the same family are not ordered, it exchanges them using the (anti)commutator: $x_1 x_2 = -x_2 x_1 + \{x_1, x_2\}$ for fermions, with the anticommutator `acmt[x1, x2]`, and $x_1 x_2 = x_2 x_1 + [x_1, x_2]$ for bosons and spin operators, with the commutator `cmt[x1, x2]`. For fermions of different families, which anticommute, it gives $x_1 x_2 = -x_2 x_1$. The repeated exchanges sort the whole product.
- The built-in rules are:

| operators | `snegOrderedQ[x1, x2]` |
|---|---|
| fermions, same symbol, `ordering` EMPTY | `OrderedQ[{x1, x2}]`: creation operators (`CR` = 0) before annihilation operators (`AN` = 1), then by the remaining indexes |
| fermions, same symbol, `ordering` SEA | `True` if *x1* is a quasiparticle creation and *x2* an annihilation operator, `False` in the opposite case, otherwise `OrderedQ[{x1, x2}]` (see `ordering`) |
| fermions, `ordering` NONE (either operator) | `True` |
| fermions, different symbols | by the roles `iscreation` and `isannihilation` of each operator in its own family (creation first); equal or unknown roles are sorted by the raw `CR`/`AN` index and `OrderedQ` |
| bosons, same symbol | `OrderedQ[{x1, x2}]`, or `True` for `ordering[a] = NONE` |
| Majorana fermions, spin operators, same symbol | `OrderedQ[{x1, x2}]` |

- For other pairs, e.g. undeclared symbols, `snegOrderedQ` remains unevaluated and the product is not reordered. Products of bosons of different symbols, of Majorana fermions of different symbols, and of operators of different kinds (bosons with fermions, spins with other operators, X-operators) are ordered by separate rules of `nc` that do not use `snegOrderedQ`.
- `nc` exchanges two operators of the same family only if `snegOrderedQ` gives `False` (more precisely, if `!snegOrderedQ[x1, x2]` evaluates to `True`); a result that is neither `True` nor `False` leaves the product unchanged.

## Developer Notes
- `snegOrderedQ` defines the canonical form of products and is the extension point for other orderings. New definitions should be given as upvalues of the operator symbol, `c /: snegOrderedQ[c[...], c[...]] := ...`, which take precedence over the built-in rules and do not affect other families. For fermions, the simplest way is often to define a new `ordering` value with `iscreation` and `isannihilation` (see `ordering`).
- The definition must be a strict total order: for $x_1 \ne x_2$ exactly one of `snegOrderedQ[x1, x2]` and `snegOrderedQ[x2, x1]` may be `False`. Otherwise `nc` exchanges the operators back and forth until the recursion limit is reached.
- Functions that evaluate expectation values or matrix elements (`vev`, `wick`, `VACUUM` rules) assume that annihilation operators stand to the right. With another canonical order they remain correct where they apply, but may leave terms unevaluated.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{snegOrderedQ[c[CR, 1, UP], c[AN, 1, UP]], snegOrderedQ[c[AN, 1, UP], c[CR, 1, UP]]}
```
`nc` exchanges the operators of the second product, which gives the contraction term:
```wl
nc[c[AN, 1, UP], c[CR, 1, UP]]
```

### Scope
Operators of the same type are sorted by their indexes, first the orbital index, then the spin (`DO` = 0 < `UP` = 1):
```wl
snegfermionoperators[c];
{snegOrderedQ[c[CR, 1, UP], c[CR, 2, DO]], snegOrderedQ[c[CR, 1, UP], c[CR, 1, DO]]}
```
---
For the Fermi sea (`ordering[d] = SEA`), the order depends on the sign of the momentum, the first index after `CR`/`AN`:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{snegOrderedQ[d[CR, 1, UP], d[AN, 1, UP]], snegOrderedQ[d[CR, -1, UP], d[AN, -1, UP]]}
```
---
Bosons and spin operators:
```wl
snegbosonoperators[a];
snegspinoperators[{S, 1/2}];
{snegOrderedQ[a[AN], a[CR]], snegOrderedQ[S[1, 3], S[1, 4]], snegOrderedQ[S[2, 3], S[1, 4]]}
```
---
For undeclared symbols, `snegOrderedQ` is not defined:
```wl
snegOrderedQ[g[AN, 1], g[CR, 1]]
```

### Applications
Anti-normal ordering of a boson, with annihilation operators to the left, as used for the Husimi Q representation. The new order is defined as an upvalue of `b`; the reordering uses the canonical commutator $[b, b^\dagger] = 1$ from `cmt`:
```wl
snegbosonoperators[b];
b /: snegOrderedQ[b[t1_, j1___], b[t2_, j2___]] := OrderedQ[{{-t1, {j1}}, {-t2, {j2}}}];
{nc[b[CR], b[AN]], nc[b[CR], b[CR], b[AN], b[AN]]}
```
The square of the number operator in anti-normal order, $(b^\dagger b)^2 = b^2 b^{\dagger 2} - 3 b b^\dagger + 1$:
```wl
Expand[pow[number[b[]], 2]]
```
---
A fermion family ordered by site first, and on each site creation before annihilation operators. This is convenient for local operators on a lattice; the anticommutators are the canonical ones:
```wl
snegfermionoperators[f];
f /: snegOrderedQ[f[t1_, i1_, s1_], f[t2_, i2_, s2_]] := OrderedQ[{{i1, t1, s1}, {i2, t2, s2}}];
nc[f[CR, 1, UP], f[CR, 2, UP], f[AN, 1, UP], f[AN, 2, UP]]
```
The hopping term $f^\dagger_1 f_2$ is then written with the site-2 operator to the right, and $f^\dagger_2 f_1 = -f_1 f^\dagger_2$:
```wl
hop[f[1], f[2], UP]
```

### Properties & Relations
For the empty vacuum, `snegOrderedQ` agrees with `OrderedQ`, since `CR` and `AN` are the integers 0 and 1:
```wl
snegfermionoperators[c];
{snegOrderedQ[c[AN, 1, UP], c[CR, 2, UP]], OrderedQ[{c[AN, 1, UP], c[CR, 2, UP]}], {CR, AN}}
```
---
Fermions of different families are ordered by their roles; with equal roles, by the symbol names:
```wl
snegfermionoperators[c, d];
{snegOrderedQ[d[CR, 1, UP], c[AN, 1, UP]], snegOrderedQ[d[CR, 1, UP], c[CR, 1, UP]], nc[d[CR, 1, UP], c[CR, 1, UP]]}
```

### Possible Issues
`vev` assumes the normal order. Products in another canonical order are not evaluated:
```wl
snegbosonoperators[b2];
b2 /: snegOrderedQ[b2[t1_, j1___], b2[t2_, j2___]] := OrderedQ[{{-t1, {j1}}, {-t2, {j2}}}];
{nc[b2[CR], b2[AN]], vev[nc[b2[CR], b2[AN]]]}
```
