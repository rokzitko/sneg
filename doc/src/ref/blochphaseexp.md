---
keywords: Bloch phase, explicit momentum components, band structure, exponential
see also: blochphase, blochhamiltonian, fourier, sneglatticevectors, ExpToTrig
---
# blochphaseexp

## Usage
- `blochphaseexp[expr, rules]` applies the substitution *rules* (e.g. `{k -> {kx, ky}, a1 -> {1, 0}}`) to *expr* and converts the Bloch phases `blochphase[k, X]` to exponentials $e^{ik\cdot X}$.
- `blochphaseexp[expr]` converts the Bloch phases without substitutions.

## Details
- After the substitution, `blochphase[k, X]` becomes `Exp[I k . X]` if both arguments are lists (vectors of equal length are converted already by `blochphase`), and `Exp[I k X]` otherwise; the latter is appropriate in one dimension with integer displacements, `blochphase[k, 1]` $\to e^{ik}$.
- The components are given in any Cartesian or lattice basis, as long as momenta and lattice vectors are written in dual bases, so that $k\cdot X$ is the phase.
- Use `ExpToTrig` and `Simplify` to obtain the dispersion in trigonometric form.

## Examples
### Basic Examples
```wl
snegmomentumindexes[{k}, Ns];
ExpToTrig[blochphaseexp[blochphase[k, 1] + blochphase[-k, 1]]]
```

### Scope
Two dimensions with explicit lattice vectors:
```wl
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1, a2];
blochphaseexp[blochphase[k, a1] + blochphase[k, a2], {k -> {kx, ky}, a1 -> {1, 0}, a2 -> {1/2, Sqrt[3]/2}}]
```

### Applications
Graphene. The honeycomb lattice has the primitive vectors $\mathbf a_1 = (1, 0)$, $\mathbf a_2 = (1/2, \sqrt3/2)$ (in units of the lattice constant) and two sites A, B per unit cell; the neighbors of an A site at $R$ are the B sites at $R$, $R - a_1$, and $R - a_2$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1, a2];
Hg = -t sum[hop[f[R, "A"], f[R, "B"]] + hop[f[R, "A"], f[R - a1, "B"]] + hop[f[R, "A"], f[R - a2, "B"]], {R}];
h = blochhamiltonian[fourier[Hg, f -> fk, R -> k], {fk["A"], fk["B"]}, k]
```
The Bloch Hamiltonian vanishes at the Dirac point $K = (4\pi/3, 0)$:
```wl
Simplify[blochphaseexp[h, {k -> {4 Pi/3, 0}, a1 -> {1, 0}, a2 -> {1/2, Sqrt[3]/2}}]]
```
The bands $\pm t |1 + e^{ik\cdot a_1} + e^{ik\cdot a_2}|$ along the line $k_y = 0$ are $\pm t (1 + 2\cos(k_x/2))$; they touch at $k_x = 4\pi/3$:
```wl
Simplify[ExpToTrig[Eigenvalues[blochphaseexp[h, {k -> {kx, 0}, a1 -> {1, 0}, a2 -> {1/2, Sqrt[3]/2}}]]], Element[kx, Reals]]
```
