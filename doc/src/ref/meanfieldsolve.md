---
keywords: self-consistent mean-field theory, Hartree-Fock, Hartree-Fock-Bogoliubov, HFB, self-consistency, gap equation, BCS, Stoner criterion, antiferromagnetism, Hubbard model, chemical potential, fixed particle number
see also: meanfield, avg, gaussianaverage, quadraticform, bogoliubov, thermalaverage
---
# meanfieldsolve

## Usage
- `meanfieldsolve[Hmf, {a1, a2, ...}, {avg[x1] -> v1, ...}]` solves the self-consistency equations for the averages `avg[...]` in the quadratic mean-field Hamiltonian *Hmf* of the modes $a_i$, starting from the given initial values, and returns an `Association` with the solution.
- `meanfieldsolve[Hmf, modes]` starts from zero for all averages.

## Details
- *Hmf* is a quadratic Hamiltonian whose coefficients depend on averages `avg[x]`, typically the result of `meanfield`. All other coefficients must be numerical; remaining symbols give the message `meanfieldsolve::numeric` and `$Failed`. The modes are specified as in `quadraticform`.
- The iteration is: the averages are inserted in *Hmf*; its BdG matrix $M$ (see `quadraticform`) gives the correlation matrix $G = \langle \Psi \Psi^\dagger \rangle$ of the thermal (or ground) state; the averages are recomputed from $G$ with Wick's theorem; and the new values are mixed linearly with the old ones, $x \to x + \alpha (x_\mathrm{new} - x)$. The iteration stops when $\max |x_\mathrm{new} - x|$ is below `"Tolerance"`, or after `"MaxIterations"` steps with the message `meanfieldsolve::conv`.
- Initial values: averages that are not given start from 0. A value can be given for an average or for its conjugate; the conjugate average gets the complex conjugate value unless it is given as well. Rules of the form `c avg[x] -> v`, as they arise from the canonical ordering of `avg` (e.g. `-avg[...]`), are converted to `avg[x] -> v/c`. Initial values for averages that do not occur in *Hmf* give the warning `meanfieldsolve::init`.
- The result is an `Association` with the keys:

| `"Averages"` | rules `avg[x] -> value` for all averages in *Hmf* |
| `"Energy"` | $\langle H_\mathrm{MF} \rangle$ in the self-consistent state, which equals the Hartree-Fock(-Bogoliubov) energy $\langle H \rangle$ for `Hmf = meanfield[H]`; with a fixed particle number, the chemical-potential term is not included |
| `"QuasiparticleEnergies"` | quasiparticle energies of $H_\mathrm{MF} - \mu N$ from `bogoliubov` (`$Failed` if not converged) |
| `"ChemicalPotential"` | $\mu$ for a fixed particle number, otherwise `None` |
| `"Iterations"` | number of iterations |
| `"Converged"` | `True` if the tolerance was reached |

- The following options can be given:

| `"Temperature"` | `0` | temperature $T$ of the thermal state ($k_B = 1$); 0 for the ground state |
| `"ParticleNumber"` | `None` | fixed fermion number $N$, adjusted with the chemical potential |
| `"Mixing"` | `1/2` | mixing parameter $\alpha$ |
| `"Tolerance"` | `10^-10` | convergence threshold for the averages |
| `"MaxIterations"` | `1000` | maximal number of iterations |

- Without `"ParticleNumber"`, the state is grand canonical with the chemical potential contained in *Hmf*.
- With `"ParticleNumber" -> N` (fermions only, $0 \le N \le n$), the term $-\mu \hat N$ is added in every iteration. For number-conserving Hamiltonians at $T = 0$, the $N$ lowest single-particle levels are filled and $\mu$ is set halfway between the highest occupied and the lowest unoccupied level; if the level at the Fermi energy is degenerate and partially filled, its states are occupied equally (an ensemble average) with the message `meanfieldsolve::open`. Otherwise $\mu$ is determined by bisection so that $\langle \hat N \rangle = N$. For bosons, `meanfieldsolve::boson` is given; an invalid $N$ gives `meanfieldsolve::number`.
- At $T = 0$, fermionic zero modes are half occupied, as in `gaussianaverage`.
- The self-consistent solution is a stationary point of the mean-field energy, not necessarily the minimum. Different initial values can converge to different solutions (e.g. paramagnetic and broken-symmetry); compare their energies. In particular, starting from zero averages preserves the symmetries of the Hamiltonian.
- The solver works with finite sets of modes. Momentum-space problems can be treated on finite clusters (`sumCluster`), or through the self-consistency conditions obtained with `bdghamiltonian` and `gaussianaverage`.

## Examples
### Basic Examples
Hartree-Fock for the half-filled Hubbard dimer, $H = -t \sum_\sigma (c_{1\sigma}^\dagger c_{2\sigma} + \mathrm{h.c.}) + U \sum_i n_{i\uparrow} n_{i\downarrow}$, at $U = 4t$, starting from an antiferromagnetic configuration:
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]] + 4 (hubbard[c[1]] + hubbard[c[2]]);
sol = meanfieldsolve[meanfield[H], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2];
Keys[sol]
```
The energy is $-t/2$, and the staggered moment is $m = \langle n_{1\uparrow} - n_{1\downarrow} \rangle = \sqrt3/2$, the solution of $m = (Um/2)/\sqrt{t^2 + (Um/2)^2}$:
```wl
{sol["Energy"], (avg[number[c[1], UP]] - avg[number[c[1], DO]]) /. sol["Averages"], sol["Converged"]}
```

### Scope
#### Paramagnetic solutions
For $U = t$ the iteration converges to the paramagnetic solution with the energy $-2t + U/2$, even from an antiferromagnetic start:
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]] + (hubbard[c[1]] + hubbard[c[2]]);
sol = meanfieldsolve[meanfield[H, "Hartree"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2];
{sol["Energy"], sol["Averages"]}
```
#### Pairing
The attractive Hubbard ring with four sites, $U = -3t$, at the chemical potential $\mu = -t/2$ and temperature $T = t/10$, in the pairing channel. Initial values of the anomalous averages $\langle c_{i\downarrow} c_{i\uparrow} \rangle$ select the superconducting solution:
```wl
snegfermionoperators[c];
H = -Sum[hop[c[i], c[Mod[i, 4] + 1]], {i, 4}] + 1/2 Sum[number[c[i]], {i, 4}] - 3 Sum[hubbard[c[i]], {i, 4}];
F = Table[avg[nc[c[AN, i, DO], c[AN, i, UP]]], {i, 4}];
sol = meanfieldsolve[meanfield[H, "Pairing"], Table[c[i], {i, 4}], Thread[F -> 0.3], "Temperature" -> 1/10];
{sol["Converged"], F /. sol["Averages"]}
```
The gap $\Delta = |U| \langle c_{i\downarrow} c_{i\uparrow} \rangle$ solves the BCS gap equation $1 = \frac{|U|}{4} \sum_k \tanh(E_k/2T)/(2E_k)$, $E_k = \sqrt{(-2\cos k + 1/2)^2 + \Delta^2}$:
```wl
{3 First[F /. sol["Averages"]], d /. FindRoot[1 == 3/4 Sum[Tanh[Sqrt[(-2 Cos[k] + 1/2)^2 + d^2]/(2/10)]/(2 Sqrt[(-2 Cos[k] + 1/2)^2 + d^2]), {k, {0, Pi/2, Pi, 3 Pi/2}}], {d, 1}]}
```
#### Bosons
Thermal Hartree-Fock for an anharmonic oscillator, $H = b^\dagger b + \frac14 b^\dagger b^\dagger b b$, at $T = 1$. The occupation satisfies $\langle n \rangle = n_B(1 + \langle n \rangle)$:
```wl
snegbosonoperators[b];
sol = meanfieldsolve[meanfield[number[b[]] + 1/4 nc[b[CR], b[CR], b[AN], b[AN]], "Normal"], {b}, {}, "Temperature" -> 1];
x = avg[number[b[]]] /. sol["Averages"];
{x, 1/(Exp[1 + x] - 1)}
```

### Options
#### ParticleNumber
The chemical potential is adjusted to give the required number of electrons. For a number-conserving Hamiltonian at $T = 0$, $\mu$ lies halfway between the highest occupied and the lowest unoccupied level:
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]] + 4 (hubbard[c[1]] + hubbard[c[2]]);
sol = meanfieldsolve[meanfield[H, "Hartree"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2];
{sol["ChemicalPotential"], sol["QuasiparticleEnergies"]}
```
The quasiparticle energies refer to $H_\mathrm{MF} - \mu N$: the single-particle levels at $\mu \pm 2$ give the energy 2 for adding an electron or a hole.
#### Temperature
At $T = 1/2$, the staggered moment of the dimer at $U = 4t$ is reduced from its ground-state value $\sqrt3/2 \approx 0.866$:
```wl
sol = meanfieldsolve[meanfield[H, "Hartree"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2, "Temperature" -> 1/2];
(avg[number[c[1], UP]] - avg[number[c[1], DO]]) /. sol["Averages"]
```
#### Mixing
A larger mixing parameter reduces the number of iterations when the iteration is stable:
```wl
Map[meanfieldsolve[meanfield[H, "Hartree"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2, "Mixing" -> #]["Iterations"] &, {1/4, 1/2, 1}]
```
#### MaxIterations
If the tolerance is not reached, a message is given and `"Converged"` is `False`:
```wl
sol = meanfieldsolve[meanfield[H, "Hartree"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2, "MaxIterations" -> 5];
{sol["Converged"], sol["QuasiparticleEnergies"]}
```

### Applications
#### Stoner criterion
In the Hartree approximation, a paramagnet becomes ferromagnetic when $U \chi_0 > 1$, where $\chi_0 = \frac{1}{N_s} \sum_k [-f'(\epsilon_k)]$ is the Pauli susceptibility per spin and site (Stoner criterion; at $T = 0$, $\chi_0$ is the density of states at the Fermi level). For a Hubbard ring with six sites and four electrons at $T = 0.1$:
```wl
L = 6; T0 = 1/10; Ne = 4;
ek = Table[-2 Cos[2 Pi j/L], {j, 0, L - 1}];
mu0 = mu /. FindRoot[2 Total[1/(Exp[(ek - mu)/T0] + 1)] == Ne, {mu, -1}];
Uc = 1/(Total[1/(4 T0 Cosh[(ek - mu0)/(2 T0)]^2)]/L)
```
Below $U_c$ the iteration converges to the paramagnetic state, above $U_c$ to a ferromagnetic state with the magnetization per site $m$:
```wl
snegfermionoperators[c];
ring[U_] := -Sum[hop[c[i], c[Mod[i, L] + 1]], {i, L}] + U Sum[hubbard[c[i]], {i, L}];
magnetization[U_] := Module[{s = meanfieldsolve[meanfield[ring[U], "Hartree"], Table[c[i], {i, L}], Flatten[Table[{avg[number[c[i], UP]] -> 0.5, avg[number[c[i], DO]] -> 0.2}, {i, L}]], "ParticleNumber" -> Ne, "Temperature" -> T0]},
  Chop[Sum[avg[number[c[i], UP]] - avg[number[c[i], DO]], {i, L}]/L /. s["Averages"], 10^-6]];
{magnetization[0.9 Uc], magnetization[1.3 Uc]}
```
#### Néel state of the Hubbard ring
The half-filled Hubbard ring with four sites at $U = 4t$ in the Hartree approximation has an antiferromagnetic solution with a staggered moment. Its energy is lower than the energy $-4t + U = 0$ of the paramagnetic state:
```wl
snegfermionoperators[c];
H = -Sum[hop[c[i], c[Mod[i, 4] + 1]], {i, 4}] + 4 Sum[hubbard[c[i]], {i, 4}];
mf = meanfield[H, "Hartree"];
neel = Flatten[Table[{avg[number[c[i], UP]] -> 0.5 + 0.4 (-1)^i, avg[number[c[i], DO]] -> 0.5 - 0.4 (-1)^i}, {i, 4}]];
afm = meanfieldsolve[mf, Table[c[i], {i, 4}], neel, "ParticleNumber" -> 4];
{Table[avg[number[c[i], UP]] - avg[number[c[i], DO]], {i, 4}] /. afm["Averages"], afm["Energy"]}
```

### Properties & Relations
The energy is the average of the interacting Hamiltonian in the self-consistent Gaussian state, computed here with `gaussianaverage` for the dimer solution (the chemical potential is included in the state):
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]] + 4 (hubbard[c[1]] + hubbard[c[2]]);
mf = meanfield[H];
sol = meanfieldsolve[mf, {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2];
M = First[quadraticform[mf /. sol["Averages"], {c[1], c[2]}]] - sol["ChemicalPotential"] DiagonalMatrix[{1, 1, 1, 1, -1, -1, -1, -1}];
{sol["Energy"], gaussianaverage[H, M, {c[1], c[2]}, 0]}
```
The Hartree-Fock energy is an upper bound of the exact ground-state energy $(U - \sqrt{U^2 + 16 t^2})/2$:
```wl
N[(4 - Sqrt[16 + 16])/2]
```

### Possible Issues
Starting from zero averages preserves the symmetry: for the dimer at $U = 4t$, the iteration then converges to the paramagnetic solution, a stationary point with a higher energy than the antiferromagnetic solution:
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]] + 4 (hubbard[c[1]] + hubbard[c[2]]);
sol = meanfieldsolve[meanfield[H, "Hartree"], {c[1], c[2]}, {}, "ParticleNumber" -> 2];
{sol["Energy"], sol["Averages"]}
```
---
For a partially filled degenerate level at $T = 0$, the degenerate states are occupied equally; the result is an ensemble average, not a pure Slater determinant. Three electrons in a tight-binding ring with four sites:
```wl
snegfermionoperators[c];
sol = meanfieldsolve[-Sum[hop[c[i], c[Mod[i, 4] + 1]], {i, 4}], Table[c[i], {i, 4}], {}, "ParticleNumber" -> 3];
sol["Energy"]
```
---
All coefficients other than the averages must be numbers:
```wl
snegfermionoperators[c];
snegrealconstants[U];
meanfieldsolve[meanfield[U hubbard[c[1]]], {c[1]}]
```
