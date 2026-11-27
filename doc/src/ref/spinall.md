---
keywords: spin operators, total spin squared, Casimir operator
see also: spinxyz, spinss, manyspin, isospinxyz
---
# spinall

## Usage
- `spinall[c[i]]` gives the list $\{S^x_i, S^y_i, S^z_i, \mathbf S_i^2\}$ for the fermion level `c[i]`.

## Details
- The first three elements are `spinxyz[c[i]]`, the last is `spinss[c[i]]`; see `spinxyz` for the conventions.
- `manyspin` gives the same list for the total spin of several levels.
- `spinall` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinall[c[]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
spinall[c[]] == Append[spinxyz[c[]], spinss[c[]]]
```
---
For a single level, `manyspin` gives the same operators in expanded form:
```wl
snegfermionoperators[c];
Expand[spinall[c[]] - manyspin[{c[]}]]
```
