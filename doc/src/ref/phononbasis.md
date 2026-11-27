---
keywords: Fock basis, oscillator cutoff, phonons, tensor product
see also: ket, bra, phononnumber, phononid, transformtoPH
---
# phononbasis

## Usage
- `phononbasis[nmax]` gives the oscillator states `ket[0]` through `ket[nmax]`.
- `phononbasis[{n1, n2, ...}]` gives the tensor-product kets of several oscillators.

## Details
- Cutoffs are nonnegative integers and include the upper state. The total dimension is the product of the individual cutoff-plus-one factors. Occupations are ordered with the last mode varying fastest.
- Use `transformtoPH` to attach these kets to each state of an electronic basis.

## Examples
### Basic Examples
```wl
phononbasis[{1,2}]
```
### Applications
The cutoff zero freezes the first oscillator in its vacuum without removing its slot:
```wl
phononbasis[{0,2}]
```
