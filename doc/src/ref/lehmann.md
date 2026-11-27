---
keywords: Lehmann representation, spectral function, poles, finite temperature, exact diagonalization, sum rule
see also: greensfunction, thermalaverage, spectralmoments, gaussianaverage, makematricesbzvc
---
# lehmann

## Usage
- `lehmann[A, B, H, modes, T]` returns the poles and residues `{{omega1,w1}, ...}` of the retarded Green's function as a list ordered by increasing frequency, from numerical diagonalization of a finite fermionic Hamiltonian.

## Details
- The propagator is $\sum_j w_j/(z-\omega_j)$. For energy eigenstates $m,n$, the frequency is $E_n-E_m$. The contribution to the residue is $\langle m|A|n\rangle\langle n|B|m\rangle$ times $\rho_m+\rho_n$ for odd *A* and $\rho_m-\rho_n$ for even *A*. The fermion parity of *A* must be definite.
- At positive *T*, $\rho_m$ are normalized Boltzmann weights with $k_B=1$; at zero temperature the ground states share equal weight. Include the chemical potential in *H* when needed. This trace includes all particle numbers, not only the half-filled sector.
- Supply a numerical Hermitian *H* and all the fermion levels appearing in *H*, *A* or *B*. Mode specifications are as for `quadraticform`; a spinful level contributes both spin orbitals. The Hamiltonian itself need not be quadratic.
- For $B=A^\dagger$ and odd *A* the residues are nonnegative. Cross correlators can have signed or complex residues. The list contains no artificial broadening. If no transition survives the tolerance the result is `{}`.

| `"Tolerance"` | `10^-9` | absolute threshold for ground-state degeneracy at zero temperature, grouping close frequencies, and discarding small individual and grouped weights |

## Developer Notes
- Diagonalization uses the full Fock space of dimension $2^n$ for *n* spin orbitals. Energy differences and weights are formed for all pairs of states. The frequency of a group is the arithmetic mean of its retained frequencies; the weight is the sum.

## Examples
### Basic Examples
At particle-hole symmetry the Hubbard atom has two equal spectral lines per spin:
```wl
snegfermionoperators[d];
h = hubbard[d[]] - number[d[]]/2;
lehmann[d[AN, UP], d[CR, UP], h, {d}, 1/3]
```

### Applications
A bonding and an antibonding orbital contribute with opposite signs to propagation from site 2 to site 1. The integrated off-diagonal weight is zero:
```wl
snegfermionoperators[c];
h = -hop[c[1], c[2]];
l = lehmann[c[AN, 1, UP], c[CR, 2, UP], h, {c[1], c[2]}, 0];
{l, Chop[Total[l[[All, 2]]]]}
```

### Properties & Relations
The retarded commutator of a conserved density with itself vanishes even when its thermal variance is nonzero:
```wl
snegfermionoperators[d];
h = hubbard[d[]] - number[d[]]/2;
lehmann[number[d[]], number[d[]], h, {d}, 1]
```
