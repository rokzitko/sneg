---
keywords: Bloch phase, plane wave, exp(ik.R), lattice sum, Fourier transformation
see also: blochphaseexp, fourier, blochhamiltonian, sumSimplifyKD, sneglatticevectors, snegmomentumindexes
---
# blochphase

## Usage
- `blochphase[k, X]` represents the Bloch phase $e^{ik\cdot X}$ of the momentum *k* and the lattice vector *X*.

## Details
- The first argument is a momentum, the second a lattice vector: a lattice index, a lattice vector declared with `sneglatticevectors`, a combination of them, or, in one dimension, an integer displacement. In output, `blochphase[k, X]` is shown as $e^{i k\cdot X}$.
- Automatic simplifications:
- `blochphase[0, X]` and `blochphase[k, 0]` are 1;
- sums in the lattice argument are split, $e^{ik\cdot(R + a_1)} = e^{ik\cdot R} e^{ik\cdot a_1}$;
- integer factors are moved to the momentum, `blochphase[k, 2 R]` is `blochphase[2 k, R]`, and integer displacements are written as `blochphase[n k, 1]`;
- products with a common lattice argument are combined, $e^{ik\cdot X} e^{iq\cdot X} = e^{i(k+q)\cdot X}$, and integer powers give $e^{ink\cdot X}$;
- `Conjugate[blochphase[k, X]]` is `blochphase[-k, X]`, and `conj` acts in the same way;
- with explicit vectors of equal length, `blochphase[{k1, k2}, {x1, x2}]` is `Exp[I (k1 x1 + k2 x2)]`.
- `blochphase` has the attribute `NumericFunction`, so Bloch phases are scalars for `nc` and `sum`.
- `sumSimplifyKD` performs the lattice sums $\sum_R e^{iK\cdot R} = N_s \delta_{K,0}$ and $\sum_k e^{ik\cdot X} = N_s \delta_{X,0}$ when the summation index appears only in Bloch phases that multiply the summand.
- `blochphaseexp` substitutes explicit components and converts the phases to exponentials. Symbolic momenta are abstract labels; the product $k\cdot X$ has no further meaning until components are given.

## Examples
### Basic Examples
```wl
snegmomentumindexes[{k, q}, Ns];
sneglatticeindexes[{R}, Ns];
blochphase[k, R]
```
```wl
blochphase[k, R] blochphase[q, R]
```

### Scope
Splitting of the lattice argument and integer displacements:
```wl
snegmomentumindexes[{k}, Ns];
sneglatticeindexes[{R}, Ns];
sneglatticevectors[a1];
{blochphase[k, R + a1], blochphase[k, R + 2], blochphase[k, 2 R]}
```
---
Conjugation and powers:
```wl
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1];
{Conjugate[blochphase[k, a1]], conj[blochphase[k, a1]], blochphase[k, a1]^3, blochphase[0, a1]}
```
---
Explicit vectors:
```wl
blochphase[{kx, ky}, {1, 0}]
```

### Applications
Lattice sums: orthogonality of plane waves, $\sum_R e^{i(k-p)\cdot R} = N_s \delta_{kp}$, and completeness, $\sum_k e^{ik\cdot(R-R')} = N_s \delta_{RR'}$:
```wl
snegmomentumindexes[{k, p}, Ns];
sneglatticeindexes[{R, Rp}, Ns];
{sumSimplifyKD[sum[blochphase[k - p, R], {R}]], sumSimplifyKD[sum[blochphase[k, R - Rp], {k}]]}
```
---
A displacement gives a phase factor after the lattice sum:
```wl
snegmomentumindexes[{k, p}, Ns];
sneglatticeindexes[{R}, Ns];
sneglatticevectors[a1];
snegcomplexfunctions[g];
sumSimplifyKD[sum[blochphase[k, R + a1] blochphase[-p, R] g[k], {R}]]
```

### Properties & Relations
`blochphaseexp` converts to exponentials; in one dimension, the lattice argument is a number:
```wl
snegmomentumindexes[{k}, Ns];
ExpToTrig[blochphaseexp[blochphase[k, 1] + blochphase[-k, 1]]]
```

### Possible Issues
Phases that do not multiply the summand, for example inside other functions, are not summed:
```wl
snegmomentumindexes[{k}, Ns];
sneglatticeindexes[{R}, Ns];
sumSimplifyKD[sum[Cos[blochphase[k, R]], {R}]]
```
