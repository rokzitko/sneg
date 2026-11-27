---
keywords: spin flip, transverse exchange, S plus S minus
see also: spinspinmp, spinspinxy, spinspin, spinplus, spinminus
---
# spinspinpm

## Usage
- `spinspinpm[a[i], b[j]]` gives the spin-flip term $S^+_{a,i} S^-_{b,j}$ between the fermion levels `a[i]` and `b[j]`.

## Details
- `spinspinpm[a, b]` is `nc[spinplus[a], spinminus[b]]`, without a factor 1/2. The transverse exchange is `spinspinxy[a, b] == (spinspinpm[a, b] + spinspinmp[a, b])/2`.
- `spinspinmp[a, b]` gives $S^-_a S^+_b$, the Hermitian conjugate.
- The levels must be given with brackets, `c[]` or `c[i]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinspinpm[c[1], c[2]]
```
It flips a down spin on level 1 and an up spin on level 2:
```wl
nc[spinspinpm[c[1], c[2]], c[CR, 1, DO], c[CR, 2, UP], VACUUM]
```

### Properties & Relations
```wl
snegfermionoperators[c];
{conj[spinspinpm[c[1], c[2]]] == spinspinmp[c[1], c[2]], Expand[(spinspinpm[c[1], c[2]] + spinspinmp[c[1], c[2]])/2 - spinspinxy[c[1], c[2]]]}
```
