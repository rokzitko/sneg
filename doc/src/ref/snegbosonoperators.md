---
keywords: boson operator, declaration, canonical commutation relations, phonon, harmonic oscillator, magnon, photon
see also: snegfermionoperators, snegspinlessfermionoperators, snegmajoranaoperators, snegspinoperators, bosonQ, operatorQ, cmt, number, ordering, listbosonoperators
---
# snegbosonoperators

## Usage
- `snegbosonoperators[a, b, ...]` declares *a*, *b*, ... as boson operators.

## Details
- A boson operator `a` is written as `a[t, j, ...]`, where the first index `t` is `CR` for the creation operator $a^\dagger_{j\ldots}$ and `AN` for the annihilation operator $a_{j\ldots}$. The further indexes, if any, label the modes (sites, momenta, polarizations) and can be numbers or symbols. A single mode is written `a[CR]`, `a[AN]`. Boson operators have no spin index.
- The operators obey the canonical commutation relations $[a_\alpha, a^\dagger_\beta] = \delta_{\alpha\beta}$, $[a_\alpha, a_\beta] = 0$, where α and β stand for all indexes. Operators of different declared boson families commute with each other, with fermions, and with Majorana fermions.
- Products are normal ordered: creation operators to the left of annihilation operators, operators of the same type sorted by their indexes and, for different families, alphabetically. Boson operators are moved to the left of fermion operators.
- `conj[a[CR, j]]` is `a[AN, j]`. `number[a[j]]` gives $a^\dagger_j a_j$. `vev` gives vacuum expectation values, and `VACUUM` stands for the boson vacuum $|0\rangle$.
- In output, `a[CR, j]` is shown as $a^\dagger_j$.
- The declaration sets `operatorQ[a]` and `bosonQ[a]` to `True`, adds `a` to `listbosonoperators`, and gives `a` the attribute `NHoldAll`.
- `ordering[a] = NONE` disables the reordering. The commutator is given by `cmt`, which can be redefined for other algebras.
- `snegbosonoperators` does not clear existing definitions of the symbols. Use symbols that have no values.

## Developer Notes
- The ordering of products of the same family is decided by `snegOrderedQ`, the commutator by `cmt`. Products of different boson families, and of bosons with other operators, are ordered by fixed rules of `nc`.

## Examples
### Basic Examples
```wl
snegbosonoperators[a];
{a[CR], a[AN], a[CR, 1]}
```
Canonical commutation relation and normal ordering:
```wl
{komutator[a[AN], a[CR]], nc[a[AN], a[AN], a[CR]]}
```

### Scope
Several modes, symbolic indexes, and several families:
```wl
snegbosonoperators[a, b];
{nc[a[AN, 1], a[CR, 2]], komutator[a[AN, k], a[CR, q]], nc[a[AN], b[CR]]}
```
---
Bosons commute with fermions and are moved to their left:
```wl
snegbosonoperators[a];
snegfermionoperators[d];
nc[d[CR, UP], a[AN], d[AN, UP], a[CR]]
```
---
Fock states and matrix elements, $\langle 0|a^3 a^{\dagger 3}|0\rangle = 3!$:
```wl
snegbosonoperators[a];
{nc[a[AN], a[CR], a[CR], VACUUM], nc[conj[VACUUM], pow[a[AN], 3], pow[a[CR], 3], VACUUM]}
```

### Applications
Position and momentum of a harmonic oscillator, $x = (a + a^\dagger)/\sqrt2$, $p = (a - a^\dagger)/(i\sqrt2)$: $[x, p] = i$ and $H = (x^2 + p^2)/2 = a^\dagger a + 1/2$:
```wl
snegbosonoperators[a];
x = (a[AN] + a[CR])/Sqrt[2];
p = (a[AN] - a[CR])/(I Sqrt[2]);
{Expand[komutator[x, p]], Expand[(pow[x, 2] + pow[p, 2])/2]}
```
---
Equation of motion of the phonon in the Holstein (or independent boson) model $H = \omega a^\dagger a + \epsilon n + g\, n (a + a^\dagger)$; the phonon is displaced by the electron occupation:
```wl
snegbosonoperators[a];
snegfermionoperators[d];
snegrealconstants[w, eps, g];
H = w number[a[]] + eps number[d[]] + g nc[number[d[]], a[CR] + a[AN]];
Expand[komutator[a[AN], H]]
```

### Properties & Relations
The declaration defines the type predicates:
```wl
snegbosonoperators[a];
{operatorQ[a], bosonQ[a], bosonQ[a[CR, 1]], MemberQ[listbosonoperators, a]}
```
---
Conjugation exchanges `CR` and `AN`:
```wl
snegbosonoperators[a];
conj[nc[a[CR, 1], a[AN, 2]]]
```

### Possible Issues
Bosons have no spin index: functions for spin-1/2 fermions, such as `hop` or `spinz`, do not apply. `number` gives the occupation of a single mode:
```wl
snegbosonoperators[a];
{number[a[]], number[a[1]]}
```
