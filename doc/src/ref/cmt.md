---
keywords: commutator, canonical commutation relations, CCR, boson algebra, su(2) Lie algebra, hard-core bosons, extending SNEG
see also: acmt, snegOrderedQ, komutator, nc, snegbosonoperators, snegspinoperators
---
# cmt

## Usage
- `cmt[op1, op2]` gives the commutator $[op_1, op_2]$ of two boson or spin operators *op1* and *op2*, which `nc` uses when it exchanges them.

## Details
- When `nc` exchanges two adjacent operators of the same boson or spin family that are not in canonical order (see `snegOrderedQ`), it uses $x_1 x_2 = x_2 x_1 + [x_1, x_2]$ with the commutator `cmt[x1, x2]`. `cmt` thus defines the algebra of the operators; `komutator` and the other functions follow from it.
- Built-in values for bosons are the canonical commutation relations $[a_\alpha, a^\dagger_\beta] = \delta_{\alpha\beta}$, $[a_\alpha, a_\beta] = [a^\dagger_\alpha, a^\dagger_\beta] = 0$, with a product of `KroneckerDelta` for the indexes (0 if the numbers of indexes differ).
- Built-in values for spin operators `S[j..., t]` (component `t` = 1, ..., 5 for $x, y, z, +, -$) are the su(2) relations on the same site, $[S^a, S^b] = i\epsilon_{abc} S^c$, $[S^z, S^\pm] = \pm S^\pm$, $[S^+, S^-] = 2S^z$, and the mixed relations such as $[S^+, S^x] = S^z$. For different sites with symbolic indexes, the commutator is multiplied by $\delta_{jj'}$. Spin operators of different symbols commute.
- Operators of different boson families commute; this is built into `nc` and does not use `cmt`.
- For undeclared symbols `cmt` remains unevaluated.

## Developer Notes
- Other commutation relations are defined by new definitions of `cmt`, preferably as upvalues of the operator symbol, `b /: cmt[b[AN, ...], b[CR, ...]] := ...`. With the normal order, `nc` only calls `cmt[b[AN, ...], b[CR, ...]]`.
- The commutator may be an operator expression. This allows Lie algebras and deformed algebras: the spin algebra is implemented in this way. The result of `cmt` is inserted into the product and reordered further, so it must be expressed in operators that are "more ordered" for the recursion to terminate.
- Additional relations, e.g. $b^2 = 0$, can be imposed with upvalues for `nc`, as in the example of hard-core bosons below.
- For fermions and Majorana fermions, the anticommutator is defined by `acmt`.

## Examples
### Basic Examples
```wl
snegbosonoperators[a];
{cmt[a[AN, 1], a[CR, 1]], cmt[a[CR, 1], a[AN, 1]], cmt[a[AN, 1], a[CR, 2]], cmt[a[AN, 1], a[AN, 2]]}
```
`nc` uses the commutator to normal order a product:
```wl
nc[a[AN], a[AN], a[CR]]
```

### Scope
Symbolic indexes:
```wl
snegbosonoperators[a];
cmt[a[AN, k], a[CR, q]]
```
---
Spin operators obey the su(2) algebra:
```wl
snegspinoperators[S];
{cmt[S[1, 1], S[1, 2]], cmt[S[1, 4], S[1, 5]], cmt[S[1, 3], S[1, 4]], cmt[S[1, 1], S[2, 2]], cmt[S[i, 1], S[j, 2]]}
```

### Applications
Hard-core bosons, $[b_i, b^\dagger_j] = \delta_{ij}(1 - 2 b^\dagger_i b_i)$ and $b_i^2 = 0$, describe spin-1/2 degrees of freedom, e.g. Cooper pairs in the strong-coupling limit, with $S^+ = b^\dagger$, $S^z = n - 1/2$. The commutator is an operator expression, and the relation $b_i^2 = 0$ is imposed as an upvalue for `nc`:
```wl
snegbosonoperators[b];
b /: cmt[b[AN, i_], b[CR, j_]] := KroneckerDelta[i, j] (1 - 2 nc[b[CR, i], b[AN, i]]);
b /: nc[x___, b[t_, i_], b[t_, i_], y___] := 0;
{nc[b[AN, 1], b[CR, 1]], nc[b[CR, 1], b[CR, 1]], nc[b[AN, 1], b[CR, 2]]}
```
The occupation number is a projector, $n^2 = n$, and on different sites the operators commute:
```wl
{Expand[pow[number[b[1]], 2]], Expand[komutator[b[AN, 1], b[CR, 2]]]}
```
The commutator $[b, b^\dagger] = 1 - 2n = -2S^z$ as for spin 1/2:
```wl
Expand[komutator[b[AN, 1], b[CR, 1]]]
```

### Properties & Relations
`komutator` computed with `nc` agrees with `cmt`:
```wl
snegbosonoperators[a];
{komutator[a[AN, k], a[CR, q]], cmt[a[AN, k], a[CR, q]]}
```
---
Bosons of different symbols commute:
```wl
snegbosonoperators[a1, a2];
{nc[a1[AN], a2[CR]], komutator[a1[AN], a2[CR]]}
```

### Possible Issues
`cmt` is not used for fermions; their anticommutator is `acmt`, and the commutator of two fermion operators is an operator:
```wl
snegfermionoperators[c];
{cmt[c[AN, 1, UP], c[CR, 1, UP]], komutator[c[AN, 1, UP], c[CR, 1, UP]]}
```
