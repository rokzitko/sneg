---
keywords: total spin squared, Casimir operator, S squared, local moment
see also: spinxyz, spinall, manyspin, spinspin, hubbard, projector1
---
# spinss

## Usage
- `spinss[c[i]]` gives the square of the spin, $\mathbf S_i^2 = (S^x_i)^2 + (S^y_i)^2 + (S^z_i)^2$, of the fermion level `c[i]`.
- `spinss[c]` is `spinss[c[]]`.

## Details
- `spinss[c[i]]` is the fourth element of `spinall[c[i]]`, computed as $\mathbf S \cdot \mathbf S$ from `spinxyz`.
- For spin-1/2 fermions, $\mathbf S^2 = \frac34 (n - 2 n_\uparrow n_\downarrow)$: the eigenvalue is $s(s+1) = 3/4$ for single occupancy and 0 for the empty and doubly occupied states.
- For fermions with spin $s \neq 1/2$ the result is returned unexpanded; apply `Expand`.
- For the total spin of several levels use `manyspin`.
- `spinss` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinss[c[]]
```
In terms of the occupation numbers:
```wl
spinss[c[]] == 3/4 (number[c[]] - 2 hubbard[c[]])
```

### Properties & Relations
$\mathbf S^2 = \frac34 P_1$, with the projector `projector1` onto single occupancy:
```wl
snegfermionoperators[c];
Expand[spinss[c[]] - 3/4 projector1[c[]]]
```
---
$\mathbf S^2$ commutes with the spin components:
```wl
snegfermionoperators[c];
Expand[komutator[spinss[c[]], #] & /@ spinxyz[c[]]]
```
---
For a spin-1 fermion level, $\mathbf S^2 = 2$ for single occupancy:
```wl
snegfermionoperators[{f, 1}];
nc[Expand[spinss[f[]]], f[CR, 0], VACUUM]
```
