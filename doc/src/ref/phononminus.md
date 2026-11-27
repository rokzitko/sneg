---
keywords: annihilation operator, phonon, oscillator truncation, commutator
see also: phononplus, phononid, phononnumber, phononx, phononbasis
---
# phononminus

## Usage
- `phononminus[nmax]` lowers the oscillator occupation by one, with matrix element $\sqrt n$.
- `phononminus[i, cutoffs]` lowers mode *i* in the product space with the given inclusive cutoffs.

## Details
- The vacuum is annihilated. The finite-dimensional commutator with `phononplus` has diagonal entries one except on the top state, where it equals minus the cutoff. The trace of the commutator is therefore zero, as for any finite matrices.

## Examples
### Basic Examples
```wl
nc[phononminus[2],#] & /@ phononbasis[2]
```
### Properties & Relations
```wl
p = phononplus[2]; m = phononminus[2];
Table[nc[bra[n],komutator[m,p],ket[n]], {n,0,2}]
```
