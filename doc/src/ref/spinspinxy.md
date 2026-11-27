---
keywords: transverse exchange, spin flip, XY model, XXZ model, anisotropic exchange
see also: spinspin, spinspinz, spinspinpm, spinspinmp, spinspinx, spinspiny
---
# spinspinxy

## Usage
- `spinspinxy[a[i], b[j]]` gives the transverse part $S^x_{a,i} S^x_{b,j} + S^y_{a,i} S^y_{b,j} = \frac12 (S^+_{a,i} S^-_{b,j} + S^-_{a,i} S^+_{b,j})$ of the exchange interaction between the fermion levels `a[i]` and `b[j]`.

## Details
- `spinspinxy[a, b]` is `(spinspinpm[a, b] + spinspinmp[a, b])/2`; see `spinxyz` for the spin operators.
- `spinspin[a, b] == spinspinz[a, b] + spinspinxy[a, b]`: the factor 1/2 is included in `spinspinxy`, but not in `spinspinpm` and `spinspinmp`.
- The levels must be given with brackets, `c[]` or `c[i]`; abstract spin operators are not supported.
- The result is not expanded.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinspinxy[c[1], c[2]]
```

### Properties & Relations
Relation with the other parts:
```wl
snegfermionoperators[c];
Expand[{spinspinxy[c[1], c[2]] - spinspinx[c[1], c[2]] - spinspiny[c[1], c[2]], spinspin[c[1], c[2]] - spinspinz[c[1], c[2]] - spinspinxy[c[1], c[2]]}]
```
---
The transverse exchange exchanges the spins of two singly occupied levels:
```wl
snegfermionoperators[c];
nc[spinspinxy[c[1], c[2]], c[CR, 1, UP], c[CR, 2, DO], VACUUM]
```
