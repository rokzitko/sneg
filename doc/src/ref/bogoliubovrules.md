---
keywords: Bogoliubov transformation, quasiparticle operators, replacement rules, BCS ground state, coherence factors, Bogoliubov-Valatin
see also: bogoliubov, quadraticform, quadraticoperator, gaussianaverage, vev
---
# bogoliubovrules

## Usage
- `bogoliubovrules[T, {a1, ..., an}, g]` returns replacement rules that express the operators $a_i$, $a_i^\dagger$ in terms of the quasiparticle operators $g_m$, $g_m^\dagger$ for the Bogoliubov transformation matrix *T*.
- `bogoliubovrules[T, {a1, ..., an}, {g1, g2, ...}]` uses the quasiparticle modes given as templates.

## Details
- *T* is the $2n \times 2n$ matrix of the transformation $\Psi = T \Phi$, $\Psi = (a_1, \ldots, a_n, a_1^\dagger, \ldots, a_n^\dagger)^T$, $\Phi = (\gamma_1, \ldots, \gamma_n, \gamma_1^\dagger, \ldots, \gamma_n^\dagger)^T$, as returned by `bogoliubov`. The rules are
$$a_i \to \sum_{m=1}^{n} (T_{im} \gamma_m + T_{i,n+m} \gamma_m^\dagger),\qquad a_i^\dagger \to \sum_{m=1}^{n} (T_{n+i,m} \gamma_m + T_{n+i,n+m} \gamma_m^\dagger).$$
- The modes $a_i$ are specified as in `quadraticform` (`c` or `c[j...]`; for fermions with spin, the spin index is added as the last index and varies fastest). They must be single operators; modes given as functions (e.g. bonding orbitals) are rejected with `bogoliubovrules::mode`.
- The quasiparticle operators $\gamma_m$ are specified as
  - a boson symbol `g`: $\gamma_m$ = `g[AN, m]`;
  - a spinless fermion symbol `g` (declared with `snegspinlessfermionoperators`): $\gamma_m$ = `g[AN, m, 0]`;
  - a list of mode templates, as for the modes $a_i$, e.g. `{gam[1]}` for a spin-1/2 fermion gives $\gamma_1 = \gamma_{1\uparrow}$, $\gamma_2 = \gamma_{1\downarrow}$.
- The number of quasiparticle modes must equal $n$; otherwise, or for a spinful fermion symbol, the message `bogoliubovrules::qp` is given. A matrix of the wrong dimension gives `quadraticoperator::dim`. In these cases `$Failed` is returned.
- The quasiparticle operators must have the same statistics as the modes $a_i$; this is not checked.
- The rules apply to expressions in which the operators appear explicitly, e.g. after `Expand`. Normal ordering of the result with respect to the quasiparticles is performed by `nc`, so `vev` of the result is the expectation value in the quasiparticle vacuum, i.e. the ground state for positive quasiparticle energies.

## Examples
### Basic Examples
On-site singlet pairing at $\xi = 4/5$, $\Delta = 3/5$; the quasiparticle energy is $E = 1$. In terms of the quasiparticles, $H = \gamma_1^\dagger \gamma_1 + \gamma_2^\dagger \gamma_2 + \xi - E$:
```wl
snegfermionoperators[c];
snegspinlessfermionoperators[g];
snegrealconstants[xi, Dl];
HS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
{eps, T, Egs} = bogoliubov[HS, {c[1]}];
rules = bogoliubovrules[T, {c[1]}, g];
Simplify[Expand[HS /. rules] /. {xi -> 4/5, Dl -> 3/5}]
```
The rules:
```wl
rules /. {xi -> 4/5, Dl -> 3/5}
```

### Scope
Spin-1/2 quasiparticles given as a template; $\gamma_{1\uparrow}$ and $\gamma_{1\downarrow}$ correspond to the two columns of $T$:
```wl
snegfermionoperators[c, gam];
snegrealconstants[xi, Dl];
HS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
{eps, T, Egs} = bogoliubov[HS, {c[1]}];
Simplify[Expand[HS /. bogoliubovrules[T, {c[1]}, {gam[1]}]] /. {xi -> 4/5, Dl -> 3/5}]
```
---
Bosons: a squeezed oscillator in terms of the quasiparticle boson `q`:
```wl
snegbosonoperators[b, q];
H = 2 number[b[]] + 1/2 (nc[b[CR], b[CR]] + nc[b[AN], b[AN]]);
{eps, T, Egs} = bogoliubov[H, {b}];
Chop[Expand[H /. bogoliubovrules[T, {b}, q]]]
```

### Applications
#### BCS ground state
The quasiparticle vacuum is the BCS ground state. The occupation and the anomalous average follow from `vev` of the transformed operators: $\langle n_\uparrow \rangle = v^2 = \frac12 (1 - \xi/E) = 1/10$ and $\langle c_\downarrow c_\uparrow \rangle = uv = \Delta/(2E) = 3/10$:
```wl
snegfermionoperators[c];
snegspinlessfermionoperators[g];
snegrealconstants[xi, Dl];
HS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
{eps, T, Egs} = bogoliubov[HS, {c[1]}];
rules = bogoliubovrules[T, {c[1]}, g];
Simplify[vev[Expand[{number[c[1], UP], nc[c[AN, 1, DO], c[AN, 1, UP]]} /. rules]] /. {xi -> 4/5, Dl -> 3/5}]
```
The same values are obtained directly with `gaussianaverage`:
```wl
gaussianaverage[{number[c[1], UP], nc[c[AN, 1, DO], c[AN, 1, UP]]}, HS /. {xi -> 4/5, Dl -> 3/5}, {c[1]}, 0]
```

### Properties & Relations
The transformation preserves the canonical anticommutation relations:
```wl
snegfermionoperators[c];
snegspinlessfermionoperators[g];
snegrealconstants[xi, Dl];
HS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
rules = bogoliubovrules[bogoliubov[HS, {c[1]}][[2]], {c[1]}, g];
Simplify[Expand[{antikomutator[c[AN, 1, UP] /. rules, c[CR, 1, UP] /. rules], antikomutator[c[AN, 1, UP] /. rules, c[AN, 1, DO] /. rules]}]]
```

### Possible Issues
For spin-1/2 quasiparticles, give a list of templates; a spinful fermion symbol is rejected:
```wl
snegfermionoperators[c, gam];
bogoliubovrules[IdentityMatrix[4], {c[1]}, gam]
```
```wl
bogoliubovrules[IdentityMatrix[4], {c[1]}, {gam[1]}]
```
---
Modes given as functions cannot be replaced by rules:
```wl
snegspinlessfermionoperators[f, g];
bogoliubovrules[IdentityMatrix[2], {Function[x, (f[x, 1, 0] + f[x, 2, 0])/Sqrt[2]]}, g]
```
