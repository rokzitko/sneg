---
keywords: normal order, Fermi sea, particle-hole transformation, quasiparticle vacuum, EMPTY, SEA, NONE, canonical order, reference state
see also: EMPTY, SEA, NONE, snegOrderedQ, iscreation, isannihilation, acmt, nc, vev, vevwick, VACUUM, snegfermionoperators
---
# ordering

## Usage
- `ordering[c] = EMPTY` selects the normal order of the fermion operators `c` with respect to the empty vacuum (default).
- `ordering[c] = SEA` selects the normal order with respect to the Fermi sea, in which the levels with negative momentum index are filled.
- `ordering[c] = NONE` disables the automatic reordering of products of the operators `c`.
- `ordering[c]` gives the current setting.

## Details
- `nc` brings products of operators to a canonical order: quasiparticle creation operators to the left of quasiparticle annihilation operators, where an annihilation operator is one that gives zero when it acts on the reference state ("vacuum") $|0\rangle$. `ordering[c]` defines this reference state for the operator family `c`, and hence which of the operators `c[CR, ...]` and `c[AN, ...]` play the role of creation and annihilation operators.
- Every declared fermion operator has `ordering[c] = EMPTY` by default. The setting is an ordinary definition and can be changed at any time; it affects the products evaluated afterwards.
- **EMPTY**: the reference state is the empty state, $c_\alpha|0\rangle = 0$ for all α. Products are ordered with `OrderedQ` applied to the operators themselves. Since `CR` = 0 < `AN` = 1, this puts $c^\dagger$ to the left of $c$, and sorts operators of the same type by their remaining indexes (orbital, then spin).
- **SEA**: the reference state is the Fermi sea $|\mathrm{FS}\rangle$. The second index of an operator `c[t, k, ...]`, the first one after `CR`/`AN`, is the momentum or energy $k$ measured from the Fermi level: the levels with $k > 0$ are empty and those with $k < 0$ are filled. The quasiparticle operators are:

| | $k > 0$ (particle) | $k < 0$ (hole) |
|---|---|---|
| creation | $c^\dagger_k$ | $c_k$ |
| annihilation | $c_k$ | $c^\dagger_k$ |

- For SEA operators, `iscreation` and `isannihilation` evaluate $k > 0$ and $k < 0$. If the sign of $k$ is not known (a symbol without assumptions, or $k = 0$), the role is undetermined and the EMPTY rule (sorting by the raw indexes) is used as fallback; the result is then correct but not normal ordered with respect to the Fermi sea. The sign of a symbolic index is declared with an upvalue, e.g. `k /: k > 0 = True`.
- For SEA operators, `vev` gives the Fermi-sea expectation values $\langle c^\dagger_{k} c_{q}\rangle = \delta_{kq}\,\theta(-k)$ and $\langle c_{k} c^\dagger_{q}\rangle = \delta_{kq}\,\theta(k)$, written with `UnitStep` (note `UnitStep[0]` = 1). Longer products are evaluated by `vevwick`, which applies Wick's theorem with these contractions. `VACUUM` stands for $|\mathrm{FS}\rangle$ in products with SEA operators.
- **NONE**: products of `c` are kept in the order in which they are entered. The Pauli rule $c_\alpha c_\alpha = 0$ and linearity still apply. A NONE operator also acts as a barrier: operators of other families are not exchanged with it. `vevwick` and `contraction` evaluate expectation values of NONE products (with the contractions from `acmt` and `cmt`); `vev` and the `VACUUM` rules do not. NONE is useful for very long operator strings or when a specific operator order is required.
- Bosons have no `ordering` value by default and are normal ordered with respect to the boson vacuum; `ordering[a] = NONE` disables the reordering of the boson operators `a`. SEA is not supported for bosons.
- Products of operators of different fermion families are ordered by the roles (`iscreation`, `isannihilation`) that each operator has in its own family, so that families with different settings can be mixed; operators of the same role are sorted with `OrderedQ`, which puts the families in alphabetical order. The exchange gives a factor $-1$ and no contraction term.
- Other values than EMPTY, SEA, and NONE can be used to define custom reference states; see Developer Notes and the example under Applications.

## Developer Notes
- The setting is read by `snegOrderedQ` (canonical order of two adjacent operators), `iscreation` and `isannihilation` (roles of the operators, used by `snegOrderedQ`, `vev`, `vevwick`, `zeroonvac`, and the `VACUUM` rules), and `contraction`.
- A custom ordering is implemented by choosing a new value, e.g. `ordering[c] = HF`, and defining for this family (preferably as upvalues of `c`, which take precedence over the built-in rules) `iscreation[c[...]]` and `isannihilation[c[...]]`, and `snegOrderedQ[c[...], c[...]]` for products within the family. The ordering of products with other families then follows automatically. The anticommutator used in exchanges is still given by `acmt`. The generic `vev` rules (an annihilation operator at the right end or a creation operator at the left end gives zero) apply to custom orderings, so `vev` of normal-ordered expressions works.
- The SEA rules for `iscreation` and `isannihilation` may be overridden in the same way, e.g. to use another index as the energy label. The SEA rules for `vev` use the second index directly.

## Examples
### Basic Examples
The default is the empty vacuum. A product $c_k c^\dagger_k$ is reordered:
```wl
snegfermionoperators[c];
{ordering[c], nc[c[AN, 1, UP], c[CR, 1, UP]]}
```
For a Fermi sea, the level $k = -1$ is filled; $c_{-1}$ creates a hole and $c^\dagger_{-1}$ annihilates it, so $c^\dagger_{-1} c_{-1}$ is reordered, and $c_{-1} c^\dagger_{-1}$ is already in normal order:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{nc[d[CR, -1, UP], d[AN, -1, UP]], nc[d[AN, -1, UP], d[CR, -1, UP]]}
```
The level $k = 2$ above the Fermi level behaves as in the empty vacuum:
```wl
nc[d[AN, 2, UP], d[CR, 2, UP]]
```

### Scope
#### EMPTY
Creation operators are moved to the left; operators of the same type are sorted by their indexes:
```wl
snegfermionoperators[c];
nc[c[AN, 1, UP], c[AN, 2, DO], c[CR, 2, UP]]
```
Annihilation operators acting on the vacuum give zero:
```wl
{nc[c[AN, 1, UP], VACUUM], nc[c[CR, 1, UP], c[CR, 1, DO], VACUUM]}
```

#### SEA
`VACUUM` stands for the Fermi sea. $c_{2\uparrow}$ and $c^\dagger_{-1\uparrow}$ annihilate it, while $c_{-1\uparrow}$ creates a hole:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{nc[d[AN, 2, UP], VACUUM], nc[d[CR, -1, UP], VACUUM], nc[d[AN, -1, UP], VACUUM]}
```
The occupation of a filled level acting on the Fermi sea:
```wl
nc[d[CR, -1, UP], d[AN, -1, UP], VACUUM]
```
Roles of the operators:
```wl
{iscreation[d[CR, 2, UP]], iscreation[d[AN, -1, UP]], isannihilation[d[CR, -1, UP]], isannihilation[d[AN, 2, UP]]}
```
---
Expectation values in the Fermi sea are written with the step function $\theta$ (`UnitStep`):
```wl
snegfermionoperators[d];
ordering[d] = SEA;
snegfreeindexes[k, q];
{vev[nc[d[CR, k, UP], d[AN, q, UP]]], vev[nc[d[CR, k, UP], d[AN, q, DO]]]}
```
Four-operator expectation values with Wick's theorem. For opposite spins only the Hartree term $n_k n_q$ remains; for equal spins the exchange (Fock) term $-\delta_{kq} n_k$ is subtracted:
```wl
{vevwick[nc[d[CR, k, UP], d[CR, q, DO], d[AN, q, DO], d[AN, k, UP]]], vevwick[nc[d[CR, k, UP], d[CR, q, UP], d[AN, q, UP], d[AN, k, UP]]]}
```
---
The sign of a symbolic momentum is declared with upvalues. Then the operators are classified and normal ordered:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
snegfreeindexes[kh, kp];
kh /: kh < 0 = True;
kp /: kp > 0 = True;
{nc[d[CR, kh, UP], d[AN, kh, UP]], nc[d[AN, kp, UP], d[CR, kh, UP]]}
```
A particle-hole pair $c^\dagger_{k_p} c_{k_h}|\mathrm{FS}\rangle$; the operators in the reverse order annihilate the Fermi sea:
```wl
{nc[d[CR, kp, UP], d[AN, kh, UP], VACUUM], nc[d[AN, kh, UP], d[CR, kp, UP], VACUUM]}
```

#### NONE
With NONE the product is kept as entered:
```wl
snegfermionoperators[c];
ordering[c] = NONE;
nc[c[AN, 1, UP], c[CR, 1, UP]]
```
The Pauli principle still applies, and `vevwick` evaluates the expectation value in the vacuum:
```wl
{nc[c[CR, 1, UP], c[CR, 1, UP]], vevwick[nc[c[AN, 1, UP], c[AN, 2, UP], c[CR, 2, UP], c[CR, 1, UP]]]}
```
Restore the default:
```wl
ordering[c] = EMPTY;
nc[c[AN, 1, UP], c[CR, 1, UP]]
```
---
NONE for bosons:
```wl
snegbosonoperators[a];
ordering[a] = NONE;
{nc[a[AN], a[CR]], vevwick[nc[a[AN], a[AN], a[CR], a[CR]]]}
```

#### Mixed families
Operators of families with different reference states are ordered by their roles. Here $c^\dagger_1$ creates a particle in the empty band, and $d^\dagger_{-1}$ annihilates a hole in the Fermi sea:
```wl
snegfermionoperators[c, d];
ordering[d] = SEA;
{nc[d[CR, -1, UP], c[CR, 1, UP]], nc[c[CR, 1, UP], d[AN, -1, UP]]}
```

### Applications
Ground-state energy of a band $H = \sum_{k\sigma} \epsilon_k n_{k\sigma}$ filled up to the Fermi level, $E_0 = 2\sum_k \epsilon_k \theta(-k)$:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
snegfreeindexes[k];
snegrealfunctions[eps];
vev[sum[eps[k] number[d[k]], {k}]]
```
---
A particle-hole excitation $|\mathrm{ph}\rangle = c^\dagger_{2\uparrow} c_{-1\uparrow}|\mathrm{FS}\rangle$ is normalized, has one electron in the level 2, and one electron (spin down) in the level −1:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
ph = nc[d[CR, 2, UP], d[AN, -1, UP], VACUUM];
{nc[conj[ph], ph], nc[conj[ph], number[d[2]], ph], nc[conj[ph], number[d[-1]], ph]}
```
---
A custom reference state: the Hartree-Fock determinant of a two-orbital molecule, with the orbital 1 doubly occupied and the orbital 2 empty. The roles of the operators `h` and the order within the family are defined as upvalues:
```wl
snegfermionoperators[h];
ordering[h] = HF;
occupied = {1};
h /: iscreation[h[t_, n_, s_]] := If[MemberQ[occupied, n], t === AN, t === CR];
h /: isannihilation[h[t_, n_, s_]] := If[MemberQ[occupied, n], t === CR, t === AN];
h /: snegOrderedQ[x1 : h[___], x2 : h[___]] := Which[iscreation[x1] && isannihilation[x2], True, isannihilation[x1] && iscreation[x2], False, True, OrderedQ[{x1, x2}]];
{nc[h[CR, 1, UP], h[AN, 1, UP]], nc[h[CR, 2, UP], h[AN, 1, UP]], nc[h[AN, 1, UP], h[CR, 2, UP]]}
```
`vev` now gives the expectation values in the reference determinant, e.g. the Hartree-Fock energy $\langle H\rangle = 2\epsilon_1 + U$:
```wl
snegrealconstants[e1, e2, U];
H = e1 number[h[1]] + e2 number[h[2]] + U (hubbard[h[1]] + hubbard[h[2]]);
{vev[number[h[1]]], vev[number[h[2]]], vev[H]}
```

### Properties & Relations
The ordering changes the representation, not the operator: with SEA and with EMPTY, the same product is written in two equivalent forms:
```wl
snegfermionoperators[c, d];
ordering[d] = SEA;
{nc[c[CR, -1, UP], c[AN, -1, UP]], nc[d[CR, -1, UP], d[AN, -1, UP]]}
```
---
The normal-ordered form of an operator with respect to the Fermi sea is obtained by subtracting the expectation value, $:\!n_k\!: = n_k - \langle n_k\rangle$:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
normalorder[nc[d[CR, -1, UP], d[AN, -1, UP]]]
```
---
`ordering` is used by `snegOrderedQ`:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{snegOrderedQ[d[AN, -1, UP], d[CR, -1, UP]], snegOrderedQ[d[CR, -1, UP], d[AN, -1, UP]]}
```

### Possible Issues
For a symbolic momentum without a declared sign, the role of the operator is unknown, and the product is ordered as for the empty vacuum:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{iscreation[d[CR, k, UP]], nc[d[AN, k, UP], d[CR, k, UP]]}
```
---
For SEA, the momentum must be the first index after `CR`/`AN`. Here the spin index is taken as the momentum: `UP` = 1 as a positive momentum (an empty level), and `DO` = 0 as a momentum with undetermined role:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{iscreation[d[CR, UP]], iscreation[d[CR, DO]], isannihilation[d[CR, DO]]}
```
---
Expressions evaluated before the change of `ordering` keep their form, because Mathematica reuses the cached result of the evaluation:
```wl
snegfermionoperators[f];
n1 = nc[f[CR, -1, UP], f[AN, -1, UP]];
ordering[f] = SEA;
{n1, nc[f[CR, -1, UP], f[AN, -1, UP]]}
```
`Update[]` discards the cached results:
```wl
Update[];
n1
```
---
`vev` does not evaluate products of NONE operators; use `vevwick`:
```wl
snegfermionoperators[c];
ordering[c] = NONE;
{vev[nc[c[AN, 1, UP], c[CR, 1, UP]]], vevwick[nc[c[AN, 1, UP], c[CR, 1, UP]]]}
```
