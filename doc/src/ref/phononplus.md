---
keywords: creation operator, phonon, truncated oscillator, ladder operator
see also: phononminus, phononnumber, phononx, phononbasis, transformtoPH
---
# phononplus

## Usage
- `phononplus[nmax]` creates an oscillator quantum in a space cut off inclusively at *nmax*.
- `phononplus[i, cutoffs]` raises mode *i* of a product of oscillators.

## Details
- On $|n\rangle$ the matrix element is $\sqrt{n+1}$ if the new state is retained, and zero at the upper cutoff. This is the projected creator, not the unbounded boson of `snegbosonoperators`.
- Modes are numbered from one; the operator acts as the identity on the other truncated mode spaces.

## Examples
### Basic Examples
```wl
nc[phononplus[2],#] & /@ phononbasis[2]
```
### Scope
```wl
nc[phononplus[2,{1,2}],ket[1,1]]
```
