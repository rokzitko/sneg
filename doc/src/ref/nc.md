---
keywords: noncommutative multiplication, operator product, normal ordering, second quantization, CenterDot
see also: komutator, antikomutator, pow, conj, snegfermionoperators, ordering, sum, vev
tech notes:
related links: [SNEG paper: Comput. Phys. Commun. 182, 2259 (2011)](https://doi.org/10.1016/j.cpc.2011.05.013)
---
# nc

## Usage
- `nc[a, b, c, ...]` represents the noncommutative product $a b c \cdots$ of the operators *a*, *b*, *c*, ....
- `nc[]` is 1, and `nc[a]` is *a*.

## Details
- `nc` is the product of operators. The ordinary product `Times` is commutative and must only be used to multiply operators by scalars.
- `nc[a, b]` can also be entered as `a ~nc~ b`. In output, products are shown as $a \cdot b$.
- `nc` is linear in each argument: it distributes over sums, and scalar factors are moved to the front. Scalars are numbers, the constants and functions declared with `snegrealconstants`, `snegcomplexconstants`, `snegrealfunctions`, ..., and expressions built from them; see `isnumericQ`.
- `nc` is associative: nested products are flattened, `nc[a, nc[b, c]]` is `nc[a, b, c]`.
- Products of declared operators are brought to a canonical order. For fermions and bosons this is the normal order: creation operators to the left of annihilation operators, and operators of the same kind sorted by their indexes. Every exchange of two fermion operators gives a factor $-1$; exchanging an annihilation and a creation operator also gives the contraction term from the canonical (anti)commutation relations $\{c_i, c_j^\dagger\} = \delta_{ij}$ and $[a_i, a_j^\dagger] = \delta_{ij}$.
- The canonical order is set for each operator family by `ordering`: `EMPTY` (the default) gives the normal order with respect to the vacuum, `SEA` with respect to the Fermi sea, and `NONE` leaves the products unordered.
- A product with two equal fermion operators vanishes (Pauli principle).
- Operators of different kinds are reordered as follows: bosons commute with fermions and are moved to the left, spin operators commute with all other operators and are moved to the left, and fermion operators of different families anticommute.
- Spin operators with a declared spin size (`snegspinoperators[{S, s}]`) are reduced to a normal form in $S^z$ and $S^\pm$ on each site.
- Products can end with a ket (`ket`, `VACUUM`) or start with a bra; annihilation operators acting on `VACUUM` give zero.
- Products of sums `sum[...]` are combined into a single sum; dummy indexes that occur in both factors are renamed.
- `nc` is `Listable`.
- Symbols that are not declared as constants or operators are treated as noncommuting operators of unknown type: they are kept in place and not reordered.
- Use `pow[a, n]` for powers of operators: `a^n` is the commutative power.

## Developer Notes
- The canonical order of two adjacent operators is decided by `snegOrderedQ`, and the (anti)commutator used to exchange them by `acmt` (fermions) and `cmt` (bosons). Redefining these for specific operators changes the algebra; for example, `ordering[c] = SEA` makes `snegOrderedQ` order particle-hole excitations of the Fermi sea.
- `nc` has no `Flat`, `OneIdentity`, or `Orderless` attributes, for performance. Patterns such as `nc[x_, y_]` therefore only match products of exactly two factors; use `nc[x___, y_, z___]` to match a factor anywhere in a product.
- Scalars are detected by `isnumericQ`; extend it with `isnumericQ[x] ^= True` for new scalar symbols, or declare them with `snegrealconstants`.
- New linear functions of operators can be declared with `sneglinearoperator`.

## Examples
### Basic Examples
Declare `c` as a fermion operator. The first index distinguishes creation (`CR`) from annihilation (`AN`) operators, and the last is the spin (`UP` or `DO`):
```wl
snegfermionoperators[c];
nc[c[CR, 1, UP], c[AN, 2, UP]]
```
Products are brought to normal order; exchanging the operators changes the sign:
```wl
nc[c[AN, 2, UP], c[CR, 1, UP]]
```
The anticommutator $\{c_{1\uparrow}, c^\dagger_{1\uparrow}\} = 1$ gives the contraction term:
```wl
nc[c[AN, 1, UP], c[CR, 1, UP]]
```
Pauli principle:
```wl
nc[c[CR, 1, UP], c[CR, 1, UP]]
```

### Scope
`nc` is linear. Declared constants are moved to the front:
```wl
snegfermionoperators[c];
snegrealconstants[t];
nc[t c[CR, 1, UP] + 2 c[CR, 2, UP], c[AN, 3, DO]]
```
---
Infix notation:
```wl
c[CR, 1, UP] ~nc~ c[AN, 1, UP]
```
---
Boson operators obey $[a, a^\dagger] = 1$:
```wl
snegbosonoperators[a];
nc[a[AN], a[CR]]
```
```wl
nc[a[AN], a[AN], a[CR], a[CR]]
```
Bosons commute with fermions:
```wl
nc[a[AN], c[CR, 1, UP], a[CR]]
```
---
Products of spin-1/2 operators are reduced, e.g. $S^+ S^- = 1/2 + S^z$:
```wl
snegspinoperators[{S, 1/2}];
nc[S[4], S[5]]
```
Spin operators commute with all other operators and are moved to the left:
```wl
nc[c[CR, 1, UP], S[1, 3]]
```
---
Operators acting on the vacuum state:
```wl
nc[c[CR, 1, UP], c[CR, 1, DO], VACUUM]
```
```wl
{nc[c[AN, 1, UP], VACUUM], nc[c[AN, 1, UP], c[CR, 1, UP], VACUUM]}
```
---
Products of sums are combined, and the dummy index of the second sum is renamed:
```wl
snegfreeindexes[k];
nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, UP], {k}]]
```
---
`nc` threads over lists:
```wl
nc[{c[CR, 1, UP], c[CR, 2, UP]}, c[AN, 1, UP]]
```

### Applications
The pair operator $b = c_\downarrow c_\uparrow$ is not a boson: $[b, b^\dagger] = 1 - n$:
```wl
snegfermionoperators[d];
b = nc[d[AN, DO], d[AN, UP]];
komutator[b, conj[b]]
```
---
For a single orbital, $(n_\uparrow - n_\downarrow)^2 = n - 2 n_\uparrow n_\downarrow$, so the local moment is maximal for single occupancy:
```wl
snegfermionoperators[d];
Expand[pow[number[d[], UP] - number[d[], DO], 2] - (number[d[]] - 2 nc[number[d[], UP], number[d[], DO]])]
```

### Properties & Relations
The empty product and the product of one factor:
```wl
snegfermionoperators[c];
{nc[], nc[c[CR, 1, UP]]}
```
---
Associativity:
```wl
nc[c[CR, 1, UP], nc[c[CR, 2, UP], c[AN, 3, UP]]] === nc[c[CR, 1, UP], c[CR, 2, UP], c[AN, 3, UP]]
```
---
Hermitian conjugation reverses the order of the factors:
```wl
conj[nc[c[CR, 1, UP], c[AN, 2, DO]]]
```
---
Internally, `CR` and `AN` are 0 and 1, and `DO` and `UP` are 0 and 1:
```wl
FullForm[nc[c[CR, 1, UP], c[AN, 2, DO]]]
```
---
`komutator` and `antikomutator` are defined with `nc`:
```wl
komutator[c[CR, 1, UP], c[AN, 2, UP]] == nc[c[CR, 1, UP], c[AN, 2, UP]] - nc[c[AN, 2, UP], c[CR, 1, UP]]
```

### Possible Issues
`Times` is commutative, so it gives wrong results for products of operators:
```wl
snegfermionoperators[c];
c[AN, 1, UP] c[CR, 1, UP] == c[CR, 1, UP] c[AN, 1, UP]
```
```wl
nc[c[AN, 1, UP], c[CR, 1, UP]] == nc[c[CR, 1, UP], c[AN, 1, UP]]
```
---
`Power` is also commutative. Use `pow`:
```wl
{c[CR, 1, UP]^2, pow[c[CR, 1, UP], 2]}
```
```wl
pow[c[CR, 1, UP] + c[AN, 1, UP], 2]
```
---
Symbols that are not declared as constants are treated as operators. The product is then not brought to normal order:
```wl
nc[c[AN, 1, UP], u c[CR, 1, UP]]
```
```wl
snegrealconstants[u];
nc[c[AN, 1, UP], u c[CR, 1, UP]]
```
