---
keywords: spin operator, y component, transverse spin, electron spin
see also: spinxyz, spinx, spinz, spinplus, spinminus, spinspiny
---
# spiny

## Usage
- `spiny[c[i]]` gives the $y$ component $S^y_i = \frac{1}{2i} (c^\dagger_{i\uparrow} c_{i\downarrow} - c^\dagger_{i\downarrow} c_{i\uparrow})$ of the spin of the fermion level `c[i]`.
- `spiny[c]` is `spiny[c[]]`.

## Details
- `spiny[c[i]]` is the second element of `spinxyz[c[i]]`; see `spinxyz` for the definition and conventions. For fermions with spin $s \neq 1/2$, $S^y = \sum_{mm'} c^\dagger_m (S^y)_{mm'} c_{m'}$ with `spinmatrixY[s]`.
- $S^y = (S^+ - S^-)/(2i)$.
- `spiny` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spiny[c[1]]
```

### Properties & Relations
$S^y = (S^+ - S^-)/(2i)$:
```wl
snegfermionoperators[c];
Expand[spiny[c[]] - (spinplus[c[]] - spinminus[c[]])/(2 I)]
```
---
Cyclic commutator $[S^y, S^z] = i S^x$:
```wl
snegfermionoperators[c];
Expand[komutator[spiny[c[]], spinz[c[]]] - I spinx[c[]]]
```
