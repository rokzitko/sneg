---
keywords: phonons, oscillator energy, occupation, number operator
see also: phononid, phononbasis, phononplus, phononminus, transformtoPH
---
# phononnumber

## Usage
- `phononnumber[nmax]` gives the number operator of one truncated oscillator.
- `phononnumber[i, cutoffs]` counts quanta in mode *i* (numbered from one).

## Details
- Its eigenvalue on a retained occupation ket is the corresponding occupation. The free oscillator energy is the frequency times this operator when the zero-point energy is dropped and $\hbar=1$.
- The product of the projected creator and annihilator is exactly the projected number operator, including on the top state. The opposite product is not the number plus identity there.

## Examples
### Basic Examples
```wl
nc[phononnumber[2,{1,3}],ket[1,2]]
```
### Properties & Relations
```wl
Expand[nc[phononplus[3],phononminus[3]] - phononnumber[3]]
```
