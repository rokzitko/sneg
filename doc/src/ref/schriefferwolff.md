---
keywords: Schrieffer-Wolff transformation, effective Hamiltonian, canonical transformation, perturbation theory, superexchange, Kondo model, Anderson model, t-J model, Heisenberg model, ring exchange
see also: eigenoperators, schriefferwolffmatrix, fermionstox, xtospin, projector1, snegxoperators, hubbard
related links: [J. R. Schrieffer and P. A. Wolff, Phys. Rev. 149, 491 (1966)](https://doi.org/10.1103/PhysRev.149.491); [S. Bravyi, D. P. DiVincenzo, and D. Loss, Ann. Phys. 326, 2793 (2011)](https://doi.org/10.1016/j.aop.2011.06.004); [Á. Bácsi, T. Iličin, and R. Žitko, Phys. Rev. B 113, 064501 (2026)](https://doi.org/10.1103/jfcl-7pzt)
---
# schriefferwolff

## Usage
- `schriefferwolff[H0, V, P, n]` gives the contributions of orders 0, 1, ..., *n* in *V* to the effective Hamiltonian $P H_\mathrm{eff} P$ obtained by the canonical Schrieffer-Wolff transformation of $H_0 + V$ with respect to the projector *P*.

## Details
- The Hamiltonian $H = H_0 + V$ is block-diagonalized with respect to $P$ and $Q = 1 - P$ by the unitary transformation $H_\mathrm{eff} = e^S H e^{-S}$, $S = S_1 + S_2 + \cdots$, with $S$ anti-Hermitian and block-off-diagonal ($P S P = Q S Q = 0$), and $S_n$ of order $V^n$. This canonical generator is unique.
- The result is the list $\{P H_0 P, P V P, P H^{(2)}_\mathrm{eff} P, \ldots, P H^{(n)}_\mathrm{eff} P\}$. At second order, $H^{(2)}_\mathrm{eff} = \frac12 P [S_1, V] P$; for a degenerate $P$ subspace with energy $E_P$, it equals $P V Q (E_P - H_0)^{-1} Q V P$.
- $H_0$ must be diagonal in the occupation numbers of the local modes, e.g. $\epsilon_d n_d + U n_{d\uparrow} n_{d\downarrow}$. All other operators in $V$ must be eigenoperators of the rest of $H_0$: band energies $\sum_k \epsilon_k n_k$, linear bosons $\omega b^\dagger b$, Zeeman terms $B S^z$, and atomic Hamiltonians $\sum_a E_a X^{aa}$ are allowed.
- $P$ is written with number operators of the local modes or diagonal X-operators, e.g. `projector1[d[]]` for single occupancy of the level `d[]`, and must satisfy $P^2 = P$.
- For translation-invariant Hamiltonians written as sums over a declared lattice index (see `sneglatticeindexes`), $P$ can be given as a rule `{c, f}`, meaning the local projector `f[c[R]]` on every site. This is supported up to second order.
- The results are operators that include the projector factors. `fermionstox` and `xtospin` convert them to X-operators and spin operators.
- The following options can be given:

| `"Generator"` | `False` | whether to also return the generators $S_1, \ldots, S_n$ |
| `"Simplify"` | `Simplify` | function applied to the coefficients |

- With `"Generator" -> True`, the result is `{list, {S1, ..., Sn}}`.
- If a component of $V$ that connects $P$ and $Q$ has a vanishing energy denominator, the message `schriefferwolff::res` is issued and `$Failed` is returned. Symbolic denominators such as $\epsilon_k - \epsilon_d$ are only rejected if they vanish identically.
- The run time grows quickly with the order and with the number of sites: the dimer to fourth order takes about a second, the 2×2 plaquette to fourth order (ring exchange) several minutes.

## Developer Notes
- Every normal-ordered operator monomial $M$ is split into eigenoperators of $H_0$ (`eigenoperators`): for a polynomial $f(n)$ in the occupation numbers, $f(n) M = M f(n + \Delta)$, so $[H_0, M] = M g(n)$ with a polynomial $g$ in the occupations of the modes not in $M$, and $M$ is resolved with projectors onto their values.
- Products are formed in an internal representation of short operator monomials times polynomials in occupation numbers, so that projectors do not generate long operator strings.
- `schriefferwolffmatrix` performs the same transformation for matrices and is used in the regression tests as an independent check.

## Examples
### Basic Examples
Superexchange in the half-filled Hubbard dimer: the projector selects single occupancy of both sites.
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H0 = U (hubbard[c[1]] + hubbard[c[2]]);
V = -t hop[c[1], c[2]];
P = nc[projector1[c[1]], projector1[c[2]]];
heff = schriefferwolff[H0, V, P, 4];
```
In terms of spin operators, the second-order term is the Heisenberg exchange $J (\mathbf S_1 \cdot \mathbf S_2 - 1/4)$ with $J = 4t^2/U$, and the fourth-order term is $-16 t^4/U^3 (\mathbf S_1 \cdot \mathbf S_2 - 1/4)$:
```wl
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
Expand[xtospin[fermionstox[#, c -> X], X -> S]] & /@ heff
```

### Scope
Anderson impurity model to Kondo model. The second-order term contains the exchange coupling $J_{kk'}$ of the impurity spin with the conduction-band spin density:
```wl
snegfermionoperators[a, d];
snegrealconstants[ed, U];
snegrealfunctions[eps, v];
snegfreeindexes[k];
H0 = sum[eps[k] number[a[k]], {k}] + ed number[d[]] + U hubbard[d[]];
V = sum[v[k] hop[a[k], d[]], {k}];
heff = schriefferwolff[H0, V, projector1[d[]], 2];
```
The spin-flip terms $J_{kk'} (S^+ s^-_{kk'} + S^- s^+_{kk'})/2$:
```wl
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
kondo = xtospin[fermionstox[heff[[3]], d -> X], X -> S];
Collect[Select[Expand[kondo /. HoldPattern[sum[x_, _]] :> x], !FreeQ[#, S[4]] &], _nc, Simplify]
```
---
Hubbard model on a chain to Heisenberg model, with the local projector on every site:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
sneglatticeindexes[{R}, Ns];
H0 = U sum[hubbard[c[R]], {R}];
V = -t sum[hop[c[R], c[R + 1]], {R}];
heff = schriefferwolff[H0, V, {c, projector1}, 2];
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
Expand[xtospin[fermionstox[heff[[3]], c -> X], X -> S]]
```

### Options
#### Generator
Also return the generators $S_1, S_2$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
P = nc[projector1[c[1]], projector1[c[2]]];
{heff, gen} = schriefferwolff[U (hubbard[c[1]] + hubbard[c[2]]), -t hop[c[1], c[2]], P, 2, "Generator" -> True];
gen[[1]]
```
The generator is anti-Hermitian:
```wl
Expand[conj[gen[[1]]] + gen[[1]]]
```
### Properties & Relations
`eigenoperators` decomposes an operator into eigenoperators of $H_0$, $O = \sum_\omega O_\omega$ with $[H_0, O_\omega] = \omega O_\omega$. The hopping in the Hubbard dimer changes the energy by $0$ or $\pm U$:
```wl
snegfermionoperators[c];
snegrealconstants[U];
eigenoperators[c[CR, 1, UP] ~nc~ c[AN, 2, UP], U (hubbard[c[1]] + hubbard[c[2]])][[All, 1]]
```
---
The fourth-order result agrees with the expansion of the exact singlet-triplet splitting of the dimer, $(\sqrt{U^2 + 16 t^2} - U)/2$:
```wl
Series[(Sqrt[U^2 + 16 t^2] - U)/2, {t, 0, 4}, Assumptions -> U > 0]
```

### Possible Issues
If $P$ and $Q$ are degenerate, the energy denominators vanish. Here a level `d` is coupled to a level `c` with $H_0 = 0$:
```wl
snegfermionoperators[c, d];
snegrealconstants[v];
schriefferwolff[0, v hop[c[], d[]], projector1[d[]], 2]
```
---
$H_0$ must be diagonal in the occupation numbers of the local modes:
```wl
snegrealconstants[U];
snegrealconstants[t];
schriefferwolff[U hubbard[c[1]] + t hop[c[1], c[2]], -t hop[c[1], c[2]], nc[projector1[c[1]], projector1[c[2]]], 2]
```
