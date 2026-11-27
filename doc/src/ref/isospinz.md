---
keywords: isospin, z component, charge, particle-hole symmetry
see also: isospin, isospinx, isospiny, isospinplus, isospinminus, isozsq, number
---
# isospinz

## Usage
- `isospinz[c[i]]` gives the $z$ component $I^z_i = \frac12 (n_i - 1)$ of the isospin of the fermion level `c[i]`.
- `isospinz[c[i], n]` is the same; the phase factor $(-1)^n$ does not affect $I^z$.
- `isospinz[c, n]` is `isospinz[c[], n]`.

## Details
- `isospinz` is the third element of `isospin`; see `isospin` for the conventions.
- $2 I^z = n - 1 = Q$ is the charge measured from half filling, the quantum number $Q$ used by `qszbasis` and `makematricesbzvc`. `isozsq` gives $(n-1)^2 = 4 (I^z)^2$.
- `isospinz` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
isospinz[c[1]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
{isospinz[c[]] == (number[c[]] - 1)/2, Expand[isozsq[c[]] - 4 nc[isospinz[c[]], isospinz[c[]]]]}
```
