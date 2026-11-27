---
keywords: spin flip, transverse exchange, S minus S plus
see also: spinspinpm, spinspinxy, spinspin, spinplus, spinminus
---
# spinspinmp

## Usage
- `spinspinmp[a[i], b[j]]` gives the spin-flip term $S^-_{a,i} S^+_{b,j}$ between the fermion levels `a[i]` and `b[j]`.

## Details
- `spinspinmp[a, b]` is `nc[spinminus[a], spinplus[b]]`, without a factor 1/2. The transverse exchange is `spinspinxy[a, b] == (spinspinpm[a, b] + spinspinmp[a, b])/2`.
- `spinspinpm[a, b]` gives $S^+_a S^-_b$, the Hermitian conjugate.
- The levels must be given with brackets, `c[]` or `c[i]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinspinmp[c[1], c[2]]
```

### Properties & Relations
$S^-_a S^+_b = S^+_b S^-_a$:
```wl
snegfermionoperators[c];
spinspinmp[c[1], c[2]] == spinspinpm[c[2], c[1]]
```
