---
keywords: isospin, charge SU(2), pseudospin, Nambu spinor
see also: isospin, isospinx, isospiny, isospinz, isospinplus, isospinminus, manyisospin, nambu
---
# isospinxyz

## Usage
- `isospinxyz[c[i]]` gives the list $\{I^x_i, I^y_i, I^z_i\}$ of the isospin operators of the fermion level `c[i]`.
- `isospinxyz[c[i], n]` uses the phase factor $(-1)^n$.

## Details
- `isospinxyz` is a synonym of `isospin`; see `isospin` for the definition $I^a = \frac12 \Psi^\dagger \sigma^a \Psi$ with $\Psi = (c_\uparrow, (-1)^n c^\dagger_\downarrow)^T$, and for the sign conventions on bipartite lattices.
- `isospinxyz` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
isospinxyz[c[1]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
isospinxyz[c[1], 1] === isospin[c[1], 1]
```
