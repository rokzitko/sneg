---
keywords: exchange, x component, XY model, anisotropic exchange
see also: spinspiny, spinspinz, spinspinxy, spinspin, spinx
---
# spinspinx

## Usage
- `spinspinx[a[i], b[j]]` gives the term $S^x_{a,i} S^x_{b,j}$ of the exchange interaction between the fermion levels `a[i]` and `b[j]`.

## Details
- `spinspinx[a, b]` is `Expand[nc[spinx[a], spinx[b]]]`. Together with `spinspiny` and `spinspinz` it describes fully anisotropic (XYZ) exchange $J_x S^x_a S^x_b + J_y S^y_a S^y_b + J_z S^z_a S^z_b$.
- The levels must be given with brackets, `c[]` or `c[i]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinspinx[c[1], c[2]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
Expand[spinspinx[c[1], c[2]] + spinspiny[c[1], c[2]] + spinspinz[c[1], c[2]] - spinspin[c[1], c[2]]]
```
