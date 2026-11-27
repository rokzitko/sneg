---
keywords: finite cluster, discrete Fourier transformation, periodic boundary conditions, twisted boundary conditions, exact diagonalization, momentum space
see also: clusterhop, sumCluster, momentumbasis, fourier, blochphase
---
# fourierrules

## Usage
- `fourierrules[c -> ck, {L1, L2, ...}]` gives replacement rules that express the site operators `c` of a finite periodic $L_1 \times L_2 \times \cdots$ cluster by the momentum operators `ck`.
- `fourierrules[c -> ck, {L1, ...}, "Twist" -> theta]` uses twisted boundary conditions.

## Details
- Sites are integer vectors in lattice coordinates, $n = (n_1, n_2, \ldots)$ with $n_a = 0, \ldots, L_a - 1$; in one dimension they are plain integers $n = 0, \ldots, L - 1$. Momentum labels are integer vectors $m$ of the same form, with the crystal momenta $k_a = (2\pi m_a + \theta_a)/L_a$.
- The rules implement the unitary transformation
$$c_{n} = \frac{1}{\sqrt{N}} \sum_m e^{i k_m\cdot n} c_{m}, \qquad c^\dagger_{n} = \frac{1}{\sqrt{N}} \sum_m e^{-i k_m\cdot n} c^\dagger_{m}, \qquad N = L_1 L_2 \cdots,$$
the finite-cluster version of the convention of `fourier`, with $k_m\cdot n = \sum_a (2\pi m_a + \theta_a) n_a / L_a$.
- The site label is the first index after `CR`/`AN` (option `"IndexPosition"`); orbital and spin indexes are kept. Only operators whose site label is a site of the cluster are replaced; other labels, such as $n = L$, are left unchanged.
- Twisted boundary conditions $c_{n + L_a e_a} = e^{i\theta_a} c_n$ shift the allowed momenta by $\theta_a / L_a$. `"Twist" -> theta` applies the same twist in all directions, `"Twist" -> {theta1, theta2, ...}` one twist per direction. The default `Automatic` means periodic boundary conditions, $\theta = 0$. Use `clusterhop` with the same twist for the real-space Hamiltonian.
- For numerical twists, the unitarity of the transformation matrix is checked; a failure gives the message `fourierrules::unitary`.
- Apply the rules with `/.` and `Expand` the result.

| `"IndexPosition"` | `1` | position of the site label among the indexes after `CR`/`AN` |
| `"Twist"` | `Automatic` | twist angles $\theta_a$ of the boundary conditions |

## Examples
### Basic Examples
A site operator of a three-site ring:
```wl
snegspinlessfermionoperators[f, fk];
f[AN, 1, 0] /. fourierrules[f -> fk, {3}]
```
The hopping on the ring becomes diagonal with the energies $-2t\cos(2\pi m/3)$:
```wl
snegrealconstants[t];
Simplify[ExpToTrig[Expand[clusterhop[-t, f, f, 1, {3}] /. fourierrules[f -> fk, {3}]]]]
```

### Scope
Two-dimensional clusters; site and momentum labels are integer vectors:
```wl
snegspinlessfermionoperators[f, fk];
f[AN, {1, 0}, 0] /. fourierrules[f -> fk, {2, 2}]
```
---
Spin-1/2 fermions; the spin index is kept:
```wl
snegfermionoperators[c, ck];
c[CR, 1, UP] /. fourierrules[c -> ck, {2}]
```

### Options
#### IndexPosition
```wl
snegspinlessfermionoperators[f, fk];
f[AN, "x", 1, 0] /. fourierrules[f -> fk, {2}, "IndexPosition" -> 2]
```
#### Twist
Antiperiodic boundary conditions, $\theta = \pi$:
```wl
snegspinlessfermionoperators[f, fk];
f[AN, 1, 0] /. fourierrules[f -> fk, {3}, "Twist" -> Pi]
```

### Applications
A twisted three-site ring, $c_{n+3} = e^{i\theta} c_n$, has the single-particle energies $-2t\cos((2\pi m + \theta)/3)$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, theta];
H = clusterhop[-t, f, f, 1, {3}, "Twist" -> theta];
Simplify[ExpToTrig[Expand[H /. fourierrules[f -> fk, {3}, "Twist" -> theta]]]]
```
---
The Hubbard ring in real space and in momentum space. The rules reproduce the momentum-space Hamiltonian obtained from the symbolic Fourier transformation with `sumCluster`:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t, U];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
HR = clusterhop[-t, c, c, 1, {4}] + U Sum[hubbard[c[n]], {n, 0, 3}];
HK = sumCluster[fourier[-t sum[hop[c[R], c[R + 1]], {R}] + U sum[hubbard[c[R]], {R}], c -> ck, R -> k], {4}];
Expand[(HR /. fourierrules[c -> ck, {4}]) - HK]
```

### Possible Issues
Site labels outside the cluster are not transformed:
```wl
snegspinlessfermionoperators[f, fk];
f[AN, 3, 0] /. fourierrules[f -> fk, {3}]
```
