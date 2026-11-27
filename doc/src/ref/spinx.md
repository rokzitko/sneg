---
keywords: spin operator, x component, transverse spin, electron spin
see also: spinxyz, spiny, spinz, spinplus, spinminus, spinspinx
---
# spinx

## Usage
- `spinx[c[i]]` gives the $x$ component $S^x_i = \frac12 (c^\dagger_{i\uparrow} c_{i\downarrow} + c^\dagger_{i\downarrow} c_{i\uparrow})$ of the spin of the fermion level `c[i]`.
- `spinx[c]` is `spinx[c[]]`.

## Details
- `spinx[c[i]]` is the first element of `spinxyz[c[i]]`; see `spinxyz` for the definition and conventions. For fermions with spin $s \neq 1/2$, $S^x = \sum_{mm'} c^\dagger_m (S^x)_{mm'} c_{m'}$ with `spinmatrixX[s]`.
- $S^x = (S^+ + S^-)/2$.
- `spinx` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinx[c[1]]
```

### Properties & Relations
$S^x = (S^+ + S^-)/2$:
```wl
snegfermionoperators[c];
Expand[spinx[c[]] - (spinplus[c[]] + spinminus[c[]])/2]
```
---
$S^x$ flips the spin of a singly occupied level:
```wl
snegfermionoperators[c];
nc[spinx[c[]], c[CR, UP], VACUUM]
```
