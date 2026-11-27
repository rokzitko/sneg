---
keywords: isospin lowering operator, pair annihilation, eta pairing, charge SU(2)
see also: isospinplus, isospin, isospinx, isospiny, isospinz, manyisospin
---
# isospinminus

## Usage
- `isospinminus[c[i]]` gives the isospin-lowering operator $I^-_i = c_{i\downarrow} c_{i\uparrow}$ of the fermion level `c[i]`, which annihilates a pair.
- `isospinminus[c[i], n]` gives $I^-_i = (-1)^n c_{i\downarrow} c_{i\uparrow}$.
- `isospinminus[c, n]` is `isospinminus[c[], n]`.

## Details
- $I^- = I^x - i I^y = (I^+)^\dagger$; see `isospin` for the conventions.
- `isospinminus` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{isospinminus[c[]], isospinminus[c[], 1]}
```
$I^-$ turns the doubly occupied state into the empty state:
```wl
nc[isospinminus[c[]], c[CR, UP], c[CR, DO], VACUUM]
```

### Properties & Relations
$I^- = I^x - i I^y$:
```wl
snegfermionoperators[c];
Expand[isospinminus[c[], 1] - (isospinx[c[], 1] - I isospiny[c[], 1])]
```
