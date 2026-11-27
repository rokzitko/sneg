---
keywords: exchange, y component, XY model, anisotropic exchange
see also: spinspinx, spinspinz, spinspinxy, spinspin, spiny
---
# spinspiny

## Usage
- `spinspiny[a[i], b[j]]` gives the term $S^y_{a,i} S^y_{b,j}$ of the exchange interaction between the fermion levels `a[i]` and `b[j]`.

## Details
- `spinspiny[a, b]` is `Expand[nc[spiny[a], spiny[b]]]`; see `spinspinx`.
- The levels must be given with brackets, `c[]` or `c[i]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinspiny[c[1], c[2]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
Expand[spinspinx[c[1], c[2]] + spinspiny[c[1], c[2]] - spinspinxy[c[1], c[2]]]
```
