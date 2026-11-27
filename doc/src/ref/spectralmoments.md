---
keywords: spectral moments, high-frequency expansion, sum rule, Hubbard bands, commutator
see also: greensfunction, liouvillian, lehmann, thermalaverage, avg
---
# spectralmoments

## Usage
- `spectralmoments[A, B, H, n]` gives the first *n*+1 coefficients in the large-frequency expansion of the retarded Green's function of *A*, *B* for *H*, starting at order $1/z$.

## Details
- The coefficient of $z^{-k-1}$ is the mean equal-time bracket of $\mathcal L^k A$ with $B$, where $\mathcal L A=[A,H]$. Use the anticommutator for odd $A$, and the commutator for even $A$. Mixed parity in *A* produces `spectralmoments::parity` and `$Failed`.
- For a diagonal fermionic propagator these are the moments of the spectral density per spin, not the moments of just its occupied part. For an ordinary canonical annihilator and its adjoint the zeroth moment is one at any temperature.
- *n* must be a nonnegative integer. Symbolic `sum` expressions are allowed; one need not close the equations of motion to determine any fixed number of moments.

| `"Average"` | `avg` | function used for equal-time averages, for instance a thermal trace on a small cluster |

## Examples
### Basic Examples
The variance of the two atomic addition/removal frequencies is $U^2 w(1-w)$ with $w$ the occupation of the opposite spin:
```wl
snegfermionoperators[d];
snegrealconstants[e, U];
h = e number[d[]] + U hubbard[d[]];
mom = spectralmoments[d[AN, UP], d[CR, UP], h, 2] /. avg[number[d[], DO]] -> w
```
```wl
Factor[mom[[3]] - mom[[2]]^2]
```

### Properties & Relations
Verify all four coefficients against a thermal Lehmann sum. For different sites the zeroth moment vanishes, while higher moments can probe hopping:
```wl
snegfermionoperators[c];
h = -hop[c[1], c[2]] + 2 Total[hubbard[{c[1], c[2]}]];
modes = {c[1], c[2]};
poles = lehmann[c[AN, 1, UP], c[CR, 2, UP], h, modes, 1/2];
mom = spectralmoments[c[AN, 1, UP], c[CR, 2, UP], h, 3, "Average" -> (thermalaverage[#, h, modes, 1/2] &)];
Max[Abs[mom - Table[Total[(Last[#] If[k == 0, 1, First[#]^k]) & /@ poles], {k, 0, 3}]]] < 10^-9
```
