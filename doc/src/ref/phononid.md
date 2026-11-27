---
keywords: oscillator identity, phonon projection, finite Hilbert space
see also: phononbasis, phononnumber, ketbratensorproduct, phononplus, phononminus
---
# phononid

## Usage
- `phononid[nmax]` gives the identity on the oscillator subspace with occupations zero through *nmax*.

## Details
- Written in ket-bra form, this is $\sum_{n=0}^{n_\mathrm{max}} |n\rangle\langle n|$. It acts as a projector if applied to states outside that subspace.
- Use an explicit identity on a spectator oscillator when constructing a tensor product of local operators.

## Examples
### Basic Examples
```wl
nc[phononid[2], #] & /@ {ket[0],ket[2],ket[3]}
```
