---
keywords: isospin, y component, pairing operator, charge SU(2)
see also: isospin, isospinx, isospinz, isospinplus, isospinminus
---
# isospiny

## Usage
- `isospiny[c[i]]` gives the $y$ component $I^y_i = \frac{1}{2i} (c^\dagger_{i\uparrow} c^\dagger_{i\downarrow} - c_{i\downarrow} c_{i\uparrow})$ of the isospin of the fermion level `c[i]`.
- `isospiny[c[i], n]` gives $I^y_i$ with the phase factor $(-1)^n$.
- `isospiny[c, n]` is `isospiny[c[], n]`.

## Details
- `isospiny` is the second element of `isospin`; see `isospin` for the conventions.
- $I^y = (I^+ - I^-)/(2i)$.
- `isospiny` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
isospiny[c[]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
Expand[isospiny[c[]] - (isospinplus[c[]] - isospinminus[c[]])/(2 I)]
```
