---
keywords: spin multiplet, spin projection, SU(2) basis, Clebsch-Gordan, NRG Ljubljana
see also: fixspinket, spindown, SU2basistensorproduct, qsbasis
---
# fixspin

## Usage
- `fixspin[v, s, sz]` gives the state with the spin projection *sz* of the multiplet with total spin *s* whose highest-weight state ($S_z = s$) is *v*.

## Details
- *v* is a state in the creation-operator representation with $S_z = s$. `fixspin` applies `spindown` $s - sz$ times; each application is normalized, so the result is the normalized state $|s, sz\rangle$ with the standard phases.
- `fixspin[v, s, s]` is *v*. For $sz > s$ the expression remains unevaluated.
- `fixspin` and `fixspinket` are used as arguments of `SU2basistensorproduct`, which builds bases with good total spin from Clebsch-Gordan combinations of multiplets; NRG Ljubljana uses them in this way.

## Examples
### Basic Examples
The $S_z = 0$ and $S_z = -1$ triplet states of two electrons:
```wl
snegfermionoperators[c];
up2 = nc[c[CR, 1, UP], c[CR, 2, UP]];
{fixspin[up2, 1, 0], fixspin[up2, 1, -1]}
```

### Properties & Relations
```wl
snegfermionoperators[c];
{fixspin[c[CR, 1, UP], 1/2, 1/2], fixspin[c[CR, 1, UP], 1/2, -1/2] == spindown[c[CR, 1, UP]]}
```
