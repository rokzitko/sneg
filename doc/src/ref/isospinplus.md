---
keywords: isospin raising operator, pair creation, eta pairing, charge SU(2)
see also: isospinminus, isospin, isospinx, isospiny, isospinz, manyisospin
---
# isospinplus

## Usage
- `isospinplus[c[i]]` gives the isospin-raising operator $I^+_i = c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}$ of the fermion level `c[i]`, which creates a pair.
- `isospinplus[c[i], n]` gives $I^+_i = (-1)^n c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}$.
- `isospinplus[c, n]` is `isospinplus[c[], n]`.

## Details
- $I^+ = I^x + i I^y$; see `isospin` for the conventions. In normal order, $c^\dagger_\uparrow c^\dagger_\downarrow$ is displayed as $-c^\dagger_\downarrow c^\dagger_\uparrow$.
- $(I^+)^\dagger = I^-$, given by `isospinminus`.
- On a bipartite lattice, $\sum_i$ `isospinplus[c[i], i]` with alternating phases is Yang's $\eta$-pairing operator; see `isospin`.
- `isospinplus` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{isospinplus[c[]], isospinplus[c[], 1]}
```
$I^+$ turns the empty state into the doubly occupied state:
```wl
nc[isospinplus[c[]], VACUUM] == nc[c[CR, UP], c[CR, DO], VACUUM]
```

### Properties & Relations
$[I^+, I^-] = 2 I^z$:
```wl
snegfermionoperators[c];
Expand[komutator[isospinplus[c[]], isospinminus[c[]]] - 2 isospinz[c[]]]
```
---
```wl
snegfermionoperators[c];
conj[isospinplus[c[], 1]] == isospinminus[c[], 1]
```
