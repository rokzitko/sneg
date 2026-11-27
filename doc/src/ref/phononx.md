---
keywords: displacement, oscillator coordinate, Holstein coupling, electron-phonon interaction
see also: phononplus, phononminus, phononnumber, phononbasis, transformtoPH
---
# phononx

## Usage
- `phononx[nmax]` gives the truncated dimensionless oscillator displacement $b+b^\dagger$.
- `phononx[i, cutoffs]` gives the displacement of mode *i* in a tensor-product basis.

## Details
- For oscillator mass $M$ and angular frequency $\omega$, multiply by $\sqrt{\hbar/(2M\omega)}$ to obtain a physical displacement. There is no factor of $1/\sqrt 2$ in the SNEG operator.
- A linear coupling mixes adjacent occupation states. Check convergence with cutoff in displaced-oscillator calculations; the top state has no raising matrix element.

## Examples
### Basic Examples
```wl
Table[nc[bra[i],phononx[2],ket[j]], {i,0,2}, {j,0,2}] // MatrixForm
```
