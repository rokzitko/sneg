---
keywords: thermal trace, Gibbs ensemble, ground-state degeneracy, exact diagonalization, double occupation, local moment
see also: lehmann, greensfunction, spectralmoments, gaussianaverage, avg
---
# thermalaverage

## Usage
- `thermalaverage[X, H, modes, T]` returns the thermal expectation of *X* in a finite fermion system by numerical diagonalization of *H*. At `T = 0` it returns the equal-weight ground-state average.

## Details
- The average is $\mathrm{Tr}(X e^{-H/T})/\mathrm{Tr}(e^{-H/T})$ at positive temperature, with $k_B=1$. Supply a numerical Hermitian Hamiltonian, including $-\mu N$ if appropriate. All charge sectors are included.
- The list of fermion modes is specified as for `quadraticform`. Both spin projections of a declared spin-1/2 level are included. Operators in *X* and *H* must refer only to the supplied modes. Unsupported modes can produce `lehmann::mode` and `$Failed`.
- Interactions are allowed. In contrast, `gaussianaverage` uses Wick's theorem for a quadratic Hamiltonian and can treat many more modes. An exact thermal trace still uses floating-point energies and eigenstates here.

| `"Tolerance"` | `10^-10` | positive absolute energy tolerance: at zero temperature, states with energy strictly below the minimum plus this tolerance are equally weighted |

## Developer Notes
- The eigensystem for a particular Hamiltonian and ordered mode list is cached in the kernel and reused for different observables or temperatures. A dense matrix on *n* spin orbitals has $2^n$ rows and $2^n$ columns.

## Examples
### Basic Examples
The ground state of the particle-hole symmetric repulsive atom is a doublet. The ensemble has zero magnetization but a finite squared moment:
```wl
snegfermionoperators[d];
h = hubbard[d[]] - number[d[]]/2;
thermalaverage[#, h, {d}, 0] & /@ {spinz[d[]], pow[spinz[d[]], 2], hubbard[d[]]}
```

### Scope
At a positive temperature even the empty and doubly occupied atomic states have weight. Use their known Boltzmann factors to verify the double occupation:
```wl
snegfermionoperators[d];
h = 2 hubbard[d[]] - number[d[]];
Abs[thermalaverage[hubbard[d[]], h, {d}, 1] - 1/(2 + 2 Exp[1])] < 10^-12
```

### Options
Resolve a small Zeeman splitting. A tolerance larger than the splitting deliberately averages the two spin projections:
```wl
snegfermionoperators[d];
h = hubbard[d[]] - number[d[]]/2 - 10^-6 spinz[d[]];
thermalaverage[spinz[d[]], h, {d}, 0, "Tolerance" -> #] & /@ {10^-10, 10^-5}
```

### Properties & Relations
For the quadratic dimer the one-body density matrix agrees with the Gaussian calculation:
```wl
snegfermionoperators[c];
modes = {c[1], c[2]}; h = -hop[c[1], c[2]];
x = nc[c[CR, 1, UP], c[AN, 2, UP]];
Abs[thermalaverage[x, h, modes, 1/2] - gaussianaverage[x, h, modes, 1/2]] < 10^-12
```
