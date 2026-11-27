---
keywords: isospin, x component, pairing operator, charge SU(2)
see also: isospin, isospiny, isospinz, isospinplus, isospinminus, bcs
---
# isospinx

## Usage
- `isospinx[c[i]]` gives the $x$ component $I^x_i = \frac12 (c^\dagger_{i\uparrow} c^\dagger_{i\downarrow} + c_{i\downarrow} c_{i\uparrow})$ of the isospin of the fermion level `c[i]`.
- `isospinx[c[i], n]` gives $I^x_i$ with the phase factor $(-1)^n$, i.e. $(-1)^n$ times the above.
- `isospinx[c, n]` is `isospinx[c[], n]`.

## Details
- `isospinx` is the first element of `isospin`; see `isospin` for the conventions.
- $I^x = (I^+ + I^-)/2$ is proportional to the local pairing operator; `bcs[c[i]]` gives $c^\dagger_\uparrow c^\dagger_\downarrow + c_\downarrow c_\uparrow = 2 I^x$.
- `isospinx` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{isospinx[c[]], isospinx[c[], 1]}
```

### Properties & Relations
$I^x = (I^+ + I^-)/2$, and `bcs` gives $2 I^x$:
```wl
snegfermionoperators[c];
{Expand[isospinx[c[]] - (isospinplus[c[]] + isospinminus[c[]])/2], Expand[bcs[c[]] - 2 isospinx[c[]]]}
```
