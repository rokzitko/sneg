---
keywords: spin operator, z component, spin projection, magnetization, Zeeman term, electron spin
see also: spinxyz, spinx, spiny, spinplus, spinminus, spinspinz, number
---
# spinz

## Usage
- `spinz[c[i]]` gives the $z$ component $S^z_i = \frac12 (n_{i\uparrow} - n_{i\downarrow})$ of the spin of the fermion level `c[i]`.
- `spinz[c]` is `spinz[c[]]`.

## Details
- `spinz[c[i]]` is the third element of `spinxyz[c[i]]`; see `spinxyz` for the definition and conventions. For fermions with spin $s \neq 1/2$, $S^z = \sum_m m\, n_m$.
- The Zeeman term of a level in a magnetic field along $z$ is proportional to $S^z$, e.g. $g \mu_B B S^z$ for electrons.
- `spinz` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinz[c[1]]
```
It equals $(n_\uparrow - n_\downarrow)/2$:
```wl
spinz[c[1]] == (number[c[1], UP] - number[c[1], DO])/2
```

### Scope
Spin-3/2 fermions:
```wl
snegfermionoperators[{f, 3/2}];
spinz[f[]]
```

### Applications
Zeeman splitting of the Hubbard atom, $H = U n_\uparrow n_\downarrow + B S^z$, in the subspaces of charge $Q$ and spin projection $S_z$:
```wl
snegfermionoperators[d];
snegrealconstants[U, B];
makematricesbzvc[U hubbard[d[]] + B spinz[d[]], qszbasisvc[{d[]}]]
```

### Properties & Relations
$S^z$ is diagonal and measures the spin projection: $[S^z, S^\pm] = \pm S^\pm$:
```wl
snegfermionoperators[c];
Expand[{komutator[spinz[c[]], spinplus[c[]]] - spinplus[c[]], komutator[spinz[c[]], spinminus[c[]]] + spinminus[c[]]}]
```
