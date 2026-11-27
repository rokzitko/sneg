---
keywords: Bloch Hamiltonian, band structure, tight-binding, h(k), Su-Schrieffer-Heeger model, graphene, Rashba spin-orbit coupling, magnons, phonons, orbital positions
see also: fourier, blochphase, blochphaseexp, bdghamiltonian, quadraticform, matrixhop, snegmomentumindexes, sneglatticevectors
---
# blochhamiltonian

## Usage
- `blochhamiltonian[H, {ck[a], ck[b], ...}, k]` gives the Bloch Hamiltonian matrix $h(k)$ of a translation-invariant bilinear Hamiltonian $H = \sum_k \sum_{\alpha\beta} c^\dagger_{k\alpha} h_{\alpha\beta}(k) c_{k\beta}$ in momentum space.

## Details
- *H* is a Hamiltonian written with momentum operators, typically the result of `fourier`, and *k* is a momentum index (`snegmomentumindexes`). The name of the summation index in *H* does not matter.
- The orbitals are given as templates: `ck` or `ck[a, ...]`, where `a, ...` are orbital indexes. The momentum is inserted as the first index after `CR`/`AN` (option `"IndexPosition"`). For fermions, the spin index is appended as the last index and varies fastest: $\{\uparrow, \downarrow\}$ (`UP`, `DO`) for spin-1/2 operators, $S, S-1, \ldots, -S$ for operators of spin $S$ (0 for spinless fermions). Boson orbitals have no spin index. The rows and columns of $h$ are ordered accordingly, e.g. $(A\uparrow, A\downarrow, B\uparrow, B\downarrow)$.
- The matrix elements are computed as $h_{\alpha\beta}(k) = \{c_{k\alpha}, [H, c^\dagger_{k\beta}]\}$ for fermions and $[b_{k\alpha}, [H, b^\dagger_{k\beta}]]$ for bosons, and simplified with `sumExpand`, `sumCollect`, and `sumSimplifyKD`. Constant terms of *H* do not contribute.
- With `"Check" -> True` (default), the matrix elements between different momenta $k \ne p$ are computed; if they do not vanish, *H* is not translation invariant for the chosen unit cell (for example a staggered potential without a doubled cell), the message `blochhamiltonian::transl` is issued, and `$Failed` is returned. A non-Hermitian $h(k)$ gives the message `blochhamiltonian::herm`, and the matrix is returned.
- Terms that do not conserve the particle number (pairing or linear terms) are not described by $h(k)$; they give the message `blochhamiltonian::anom` and `$Failed`. Use `bdghamiltonian` for the Bogoliubov-de Gennes form. Interaction terms leave operators in the matrix elements and give `blochhamiltonian::rest`.
- The default convention is $c_{R\alpha} = N_s^{-1/2} \sum_k e^{ik\cdot R} c_{k\alpha}$, with the phase of the unit cell $R$ for all orbitals in the cell (as in `fourier`). With `"OrbitalPositions" -> {tau_a, tau_b, ...}`, one position per orbital template, the matrix is returned in the convention $c_{R\alpha} = N_s^{-1/2} \sum_k e^{ik\cdot(R+\tau_\alpha)} c_{k\alpha}$, $h^{\mathrm{II}}_{\alpha\beta} = e^{-ik\cdot\tau_\alpha} h_{\alpha\beta} e^{ik\cdot\tau_\beta}$. Both matrices have the same eigenvalues; the second convention is needed for Berry phases and polarization.
- The result contains Bloch phases `blochphase[k, X]`; `blochphaseexp` substitutes explicit components.

| `"Check"` | `True` | whether to check translation invariance and Hermiticity |
| `"IndexPosition"` | `1` | position of the momentum among the indexes after `CR`/`AN` |
| `"OrbitalPositions"` | `None` | positions $\tau_\alpha$ of the orbitals in the unit cell |

| `blochhamiltonian::index` | *k* is not a momentum index |
| `blochhamiltonian::orb` | an orbital template is not a fermion or boson operator |
| `blochhamiltonian::anom` | *H* contains terms that do not conserve the particle number |
| `blochhamiltonian::rest` | a matrix element contains operators or unevaluated sums |
| `blochhamiltonian::transl` | *H* is not translation invariant for the given orbitals |
| `blochhamiltonian::herm` | $h(k)$ is not Hermitian (warning) |

## Examples
### Basic Examples
Tight-binding chain, $\epsilon_k = -2t\cos k$ for both spin directions:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hk = fourier[-t sum[hop[c[R], c[R + 1]], {R}], c -> ck, R -> k];
h = blochhamiltonian[Hk, {ck}, k]
```
```wl
ExpToTrig[blochphaseexp[h]]
```

### Scope
Square lattice of spinless fermions:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1, a2];
H = -t sum[hop[f[R], f[R + a1]] + hop[f[R], f[R + a2]], {R}];
h = blochhamiltonian[fourier[H, f -> fk, R -> k], {fk}, k];
Simplify[ExpToTrig[blochphaseexp[h, {k -> {kx, ky}, a1 -> {1, 0}, a2 -> {0, 1}}]]]
```
---
A Zeeman field, $B \sum_R S^z_R$, splits the spin-up and spin-down bands:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t, B];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = -t sum[hop[c[R], c[R + 1]], {R}] + B sum[spinz[c[R]], {R}];
blochhamiltonian[fourier[H, c -> ck, R -> k], {ck}, k]
```
---
Bosons. Hopping of bosons (e.g. magnons or a tight-binding phonon branch), $\omega_k = \omega_0 + 2J\cos k$:
```wl
snegbosonoperators[b, bk];
snegrealconstants[w0, J];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = sum[w0 nc[b[CR, R], b[AN, R]] + J (nc[b[CR, R], b[AN, R + 1]] + nc[b[CR, R + 1], b[AN, R]]), {R}];
blochhamiltonian[fourier[H, b -> bk, R -> k], {bk}, k]
```

### Options
#### Check
Without the checks, the translation invariance and the Hermiticity are not tested, which saves time for large unit cells:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
blochhamiltonian[fourier[-t sum[hop[c[R], c[R + 1]], {R}], c -> ck, R -> k], {ck}, k, "Check" -> False]
```
#### IndexPosition
The momentum is the second index after `CR`/`AN`:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hx = fourier[-t sum[hop[f["x", R], f["x", R + 1]], {R}], f -> fk, R -> k, "IndexPosition" -> 2];
blochhamiltonian[Hx, {fk["x"]}, k, "IndexPosition" -> 2]
```
#### OrbitalPositions
Su-Schrieffer-Heeger chain with the B site in the middle of the unit cell:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[v, w];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hssh = fourier[sum[v hop[f[R, "A"], f[R, "B"]] + w hop[f[R + 1, "A"], f[R, "B"]], {R}], f -> fk, R -> k];
blochhamiltonian[Hssh, {fk["A"], fk["B"]}, k, "OrbitalPositions" -> {0, 1/2}]
```

### Applications
Su-Schrieffer-Heeger chain. The intracell hopping $v$ and the intercell hopping $w$ give the off-diagonal Bloch matrix with $h_{AB}(k) = v + w e^{-ik}$, $h_{BA}(k) = h_{AB}(k)^*$, and the bands $\pm\sqrt{v^2 + w^2 + 2vw\cos k}$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[v, w];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hssh = fourier[sum[v hop[f[R, "A"], f[R, "B"]] + w hop[f[R + 1, "A"], f[R, "B"]], {R}], f -> fk, R -> k];
h = blochhamiltonian[Hssh, {fk["A"], fk["B"]}, k]
```
```wl
Simplify[Eigenvalues[ExpToTrig[blochphaseexp[h]]]]
```
---
Graphene, with the B neighbors of an A site at $R$, $R - a_1$, and $R - a_2$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1, a2];
Hg = -t sum[hop[f[R, "A"], f[R, "B"]] + hop[f[R, "A"], f[R - a1, "B"]] + hop[f[R, "A"], f[R - a2, "B"]], {R}];
h = blochhamiltonian[fourier[Hg, f -> fk, R -> k], {fk["A"], fk["B"]}, k]
```
The gap closes at the Dirac point $K = (4\pi/3, 0)$:
```wl
Simplify[blochphaseexp[h, {k -> {4 Pi/3, 0}, a1 -> {1, 0}, a2 -> {1/2, Sqrt[3]/2}}]]
```
---
Rashba chain. Spin-dependent hopping $\sum_R c^\dagger_R (-t + i\alpha\sigma_y) c_{R+1} + \mathrm{h.c.}$, written with `matrixhop`, gives $h(k) = -2t\cos k - 2\alpha\sin k\,\sigma_y$:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t, alpha];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = sum[matrixhop[-t IdentityMatrix[2] + I alpha PauliMatrix[2], c[R], c[R + 1]], {R}];
h = blochhamiltonian[fourier[H, c -> ck, R -> k], {ck}, k];
Simplify[ExpToTrig[blochphaseexp[h]]]
```
---
Charge-density wave with a doubled unit cell. A staggered potential $\pm\Delta$ on the sublattices A and B opens a gap $2\Delta$ at the zone boundary:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, Delta];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hcdw = sum[-t hop[f[R, "A"], f[R, "B"]] - t hop[f[R, "B"], f[R + 1, "A"]] + Delta (number[f[R, "A"]] - number[f[R, "B"]]), {R}];
h = blochhamiltonian[fourier[Hcdw, f -> fk, R -> k], {fk["A"], fk["B"]}, k];
Simplify[Eigenvalues[ExpToTrig[blochphaseexp[h]]]]
```

### Properties & Relations
The eigenvalues do not depend on the orbital positions:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[v, w];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hssh = fourier[sum[v hop[f[R, "A"], f[R, "B"]] + w hop[f[R + 1, "A"], f[R, "B"]], {R}], f -> fk, R -> k];
h1 = blochhamiltonian[Hssh, {fk["A"], fk["B"]}, k];
h2 = blochhamiltonian[Hssh, {fk["A"], fk["B"]}, k, "OrbitalPositions" -> {0, 1/2}];
Simplify[blochphaseexp[Det[h1] - Det[h2]]]
```

### Possible Issues
A staggered potential is not translation invariant for a unit cell with one site:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[Delta];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
blochhamiltonian[fourier[Delta sum[blochphase[Pi, R] number[f[R]], {R}], f -> fk, R -> k], {fk}, k]
```
---
Pairing terms are rejected; use `bdghamiltonian`:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[Delta];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
blochhamiltonian[fourier[Delta sum[nc[f[CR, R], f[CR, R + 1]] + nc[f[AN, R + 1], f[AN, R]], {R}], f -> fk, R -> k], {fk}, k]
```
---
Non-Hermitian matrices are reported:
```wl
snegspinlessfermionoperators[fk];
snegmomentumindexes[{k}, Ns];
blochhamiltonian[sum[I nc[fk[CR, k, "A", 0], fk[AN, k, "B", 0]], {k}], {fk["A"], fk["B"]}, k]
```
---
*H* must be written with the momentum operators. A real-space Hamiltonian gives a zero matrix without a message:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
blochhamiltonian[-t sum[hop[c[R], c[R + 1]], {R}], {ck}, k]
```
