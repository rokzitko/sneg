---
keywords: fermion operator, declaration, creation operator, annihilation operator, canonical anticommutation relations, electron
see also: snegspinlessfermionoperators, snegbosonoperators, snegmajoranaoperators, snegspinoperators, snegrealconstants, fermionQ, operatorQ, ordering, nc, number
---
# snegfermionoperators

## Usage
- `snegfermionoperators[c, d, ...]` declares *c*, *d*, ... as spin-1/2 fermion operators.
- `snegfermionoperators[{c, s}, ...]` declares *c* as a fermion operator with spin *s*.

## Details
- An operator `c` is written as `c[t, i, j, ..., σ]`. The first index `t` is `CR` for the creation operator $c^\dagger_{ij\ldots\sigma}$ and `AN` for the annihilation operator $c_{ij\ldots\sigma}$. The last index is the spin projection σ, `UP` or `DO` for spin 1/2; the indexes in between label the orbitals, sites, or momenta and can be numbers or symbols.
- The operators obey the canonical anticommutation relations $\{c_\alpha, c^\dagger_\beta\} = \delta_{\alpha\beta}$, $\{c_\alpha, c_\beta\} = 0$, where α and β stand for all indexes. Operators of different declared fermion families anticommute.
- Functions that construct operators, such as `number`, `hop`, or `hubbard`, take the level without the first and last indexes: `c[i]` or `c[]`.
- For spin *s*, the spin index takes the values $-s, -s+1, \ldots, s$. Fermions with spin 0 (spinless fermions) are declared with `snegspinlessfermionoperators`.
- In output, `c[CR, i, UP]` is shown as $c^\dagger_{i\uparrow}$.
- The declaration sets `operatorQ[c]` and `fermionQ[c]` to `True`. All declared fermion operators are listed in `listfermionoperators`.
- The products are normal ordered with respect to the vacuum. Set `ordering[c] = SEA` for the particle-hole ordering with respect to the Fermi sea.
- The declared symbols get the attribute `NHoldAll`, so that `N` does not change the indexes.
- `snegfermionoperators` does not clear existing definitions of the symbols. Declare operators before using them; use symbols that have no values.

## Developer Notes
- The declaration is implemented by `snegaddop`, which sets the upvalues `operatorQ[c] ^= True`, `fermionQ[c] ^= True`, and `spinof[c] ^= s`, and adds the output formatting rules if `PrettyOutput` is `True` (the default). Code that tests operator types should use `fermionQ`, `bosonQ`, `spinQ`, and `spinof`.
- The canonical order is decided by `snegOrderedQ` and the anticommutator by `acmt`; both can be redefined for a declared family to implement other algebras.

## Examples
### Basic Examples
Declare `c` and `d` as fermion operators:
```wl
snegfermionoperators[c, d];
{c[CR, 1, UP], c[AN, 1, UP], d[CR, DO]}
```
Canonical anticommutation relations:
```wl
{antikomutator[c[AN, 1, UP], c[CR, 1, UP]], antikomutator[c[AN, 1, UP], c[CR, 2, UP]], antikomutator[c[AN, 1, UP], d[CR, UP]]}
```

### Scope
Operators with several orbital indexes and with a symbolic spin index:
```wl
snegfermionoperators[c];
{c[CR, k, 2, DO], c[AN, i, σ]}
```
---
Operators with spin 3/2 have four spin components, $\sigma = -3/2, \ldots, 3/2$:
```wl
snegfermionoperators[{f, 3/2}];
number[f[]]
```
---
Spinless fermions are declared with `snegspinlessfermionoperators`:
```wl
snegspinlessfermionoperators[a];
nc[a[AN, 1], a[CR, 1]]
```
---
`listfermionoperators` contains all declared fermion operators:
```wl
listfermionoperators
```

### Properties & Relations
The declaration defines the operator type predicates:
```wl
snegfermionoperators[c];
{operatorQ[c], fermionQ[c], spinof[c]}
```
---
The attribute `NHoldAll` protects the indexes from `N`:
```wl
N[Sqrt[2] c[CR, 1, UP]]
```

### Possible Issues
Undeclared symbols are not operators; their products are not reordered:
```wl
nc[g[AN, 1, UP], g[CR, 1, UP]]
```
```wl
snegfermionoperators[g];
nc[g[AN, 1, UP], g[CR, 1, UP]]
```
---
`number`, `hubbard`, and similar functions expect the level, not the operator. Here `CR` and `UP` are taken as level indexes:
```wl
snegfermionoperators[c];
number[c[CR, 1, UP]]
```
The number operator of the level `c[1]`, and of its spin-up component:
```wl
{number[c[1]], number[c[1], UP]}
```
