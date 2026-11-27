---
keywords: spin raising operator, ladder operator, spin flip, S plus
see also: spinminus, spinxyz, spinx, spiny, spinz, spinspinpm, spindown
---
# spinplus

## Usage
- `spinplus[c[i]]` gives the spin-raising operator $S^+_i = S^x_i + i S^y_i = c^\dagger_{i\uparrow} c_{i\downarrow}$ of the fermion level `c[i]`.
- `spinplus[c]` is `spinplus[c[]]`.

## Details
- See `spinxyz` for the definition of the spin operators. For fermions with spin $s \neq 1/2$, $S^+ = \sum_m \sqrt{s(s+1) - m(m+1)}\, c^\dagger_{m+1} c_m$.
- $(S^+)^\dagger = S^-$, given by `spinminus`.
- `spinplus` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinplus[c[1]]
```
$S^+$ raises the spin projection of a singly occupied level and annihilates the spin-up state:
```wl
{nc[spinplus[c[1]], c[CR, 1, DO], VACUUM], nc[spinplus[c[1]], c[CR, 1, UP], VACUUM]}
```

### Scope
Spin-1 fermions:
```wl
snegfermionoperators[{f, 1}];
spinplus[f[]]
```

### Properties & Relations
$[S^+, S^-] = 2 S^z$:
```wl
snegfermionoperators[c];
Expand[komutator[spinplus[c[]], spinminus[c[]]] - 2 spinz[c[]]]
```
---
Hermitian conjugate:
```wl
snegfermionoperators[c];
conj[spinplus[c[]]] == spinminus[c[]]
```
