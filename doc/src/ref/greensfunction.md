---
keywords: retarded Green's function, equations of motion, Zubarev, spectral function, resonant level, Hubbard atom
see also: eomclosure, liouvillian, spectralmoments, thermalaverage, lehmann, gaussianaverage, avg
---
# greensfunction

## Usage
- `greensfunction[A, B, H, z]` gives the retarded Green's function of *A* and *B* for the Hamiltonian *H* at complex frequency *z*, by solving the closed equations of motion.

## Details
- Time evolution is $A(t)=e^{iHt} A e^{-iHt}$ with $\hbar=1$. The retarded kernel is $-i\theta(t)$ times the expectation value of the anticommutator for odd *A*, or the commutator for even *A*. The Fourier convention is $e^{izt}$ with positive imaginary part of $z$. Give *B* explicitly: it is not automatically conjugated.
- For `eomclosure[A,H]` equal to `{ops,M,a}`, the result is $a\cdot(z I-M)^{-1}N$ with $N_i$ the equal-time bracket of `ops[[i]]` with *B*, averaged in the chosen state. No factorization is made.
- For $B=A^\dagger$ the spectral density is $-\mathrm{Im}\,G(\omega+i0^+)/\pi$. On a finite cluster it consists of discrete lines. Use `lehmann` for their frequencies and weights.
- Use explicit levels, not symbolic `sum` expressions. *A* must have definite fermion parity. A failed closure returns `$Failed`, not a truncated Green's function.

| `"Average"` | `avg` | function applied to each equal-time bracket; by default residual expectation values are symbolic |
| `"MaxOperators"` | `100` | maximum number of distinct operator monomials in the closure |

## Developer Notes
- With exact coefficients in the closure matrix, the frequency dependence is obtained by a transposed linear solve; numerical averages enter only in the last contraction. For inexact matrices an eigendecomposition is used.

## Examples
### Basic Examples
The two poles of the Hubbard atom are separated by the repulsion $U$. Their weights sum to one per spin:
```wl
snegfermionoperators[d];
snegrealconstants[e, U];
h = e number[d[]] + U hubbard[d[]];
g = greensfunction[d[AN, UP], d[CR, UP], h, z];
Apart[g /. avg[number[d[], DO]] -> w, z]
```

### Scope
A transverse spin response uses a commutator. For $H=B S^z$ the raising operator evolves with $e^{iBt}$, hence the pole is at $z=-B$:
```wl
snegspinoperators[{S, 1/2}];
snegrealconstants[B];
greensfunction[S[1, 4], S[1, 5], B S[1, 3], z]
```

### Options
Resolve the symbolic averages by an exact thermal trace. At particle-hole symmetry both atomic transitions carry equal weight:
```wl
snegfermionoperators[d];
h = -number[d[]]/2 + hubbard[d[]];
g = greensfunction[d[AN, UP], d[CR, UP], h, z, "Average" -> (thermalaverage[#, h, {d}, 1/3] &)];
Abs[(g - z/(z^2 - 1/4)) /. z -> 1/3 + I/2] < 10^-12
```

### Applications
A level hybridized to one bath orbital has a frequency-dependent self-energy $v^2/(z-e_b)$ even without an interaction:
```wl
snegfermionoperators[d, c];
snegrealconstants[ed, eb, v];
h = ed number[d[]] + eb number[c[]] + v hop[d[], c[]];
g = greensfunction[d[AN, UP], d[CR, UP], h, z];
Factor[z - ed - 1/g]
```
