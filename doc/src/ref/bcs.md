---
keywords: BCS pairing, superconductivity, pair creation, s-wave pairing, order parameter, superconducting phase, Andreev bound states, Yu-Shiba-Rusinov states, zero-bandwidth model, Josephson junction, isospin
see also: anhop, genanhop, anomaloushop, projectorEphi, hopphi, isospinx, nambu, twohop, bogoliubov
related links: [J. Bardeen, L. N. Cooper, and J. R. Schrieffer, Phys. Rev. 108, 1175 (1957)](https://doi.org/10.1103/PhysRev.108.1175); [T. Iličin and R. Žitko, SciPost Phys. 19, 006 (2025) (superconducting Anderson impurity model)](https://doi.org/10.21468/SciPostPhys.19.1.006)
---
# bcs

## Usage
- `bcs[c[i]]` gives the local pairing operator $c^\dagger_{i\uparrow} c^\dagger_{i\downarrow} + c_{i\downarrow} c_{i\uparrow}$.
- `bcs[c[i], Φ]` gives $e^{i\Phi} c^\dagger_{i\uparrow} c^\dagger_{i\downarrow} + e^{-i\Phi} c_{i\downarrow} c_{i\uparrow}$ with the phase *Φ*.
- `bcs[Δ, c[i]]` gives $\Delta\, c^\dagger_{i\uparrow} c^\dagger_{i\downarrow} + \Delta^*\, c_{i\downarrow} c_{i\uparrow}$ with the complex amplitude *Δ*.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes. The order of the arguments distinguishes the two forms: a level as the first argument means `bcs[c[i], Φ]`, a level as the second argument means `bcs[Δ, c[i]]`.
- The pair creation operator is $c^\dagger_\uparrow c^\dagger_\downarrow$, with the spin-up operator on the left. The doubly occupied state used in this convention is $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow |0\rangle$.
- The mean-field (BCS) pairing term of a superconductor with the gap $\Delta$ and the phase $\Phi$ in a local (Wilson-chain or zero-bandwidth) representation is $-\Delta \sum_i$ `bcs[c[i], Φ]`. For a single level $H = \xi\,(n - 1) - \Delta\, \mathrm{bcs}(c, \Phi)$, the even states $|0\rangle$, $|2\rangle$ are mixed into the energies $\pm\sqrt{\xi^2 + \Delta^2}$, and the singly occupied states have the energy 0: the quasiparticle excitation energy is $E = \sqrt{\xi^2 + \Delta^2}$. The ground state for $\xi = 0$ is $(|0\rangle + e^{i\Phi}|2\rangle)/\sqrt{2}$, see `projectorEphi`.
- `bcs[c[i], Φ]` is $2(\cos\Phi\, I^x - \sin\Phi\, I^y)$ in terms of the isospin operators of the level (`isospinx`, `isospiny`); `bcs[c[i]]` is $2 I^x$. It is a spin singlet: it commutes with the spin operators of the level.
- Under the gauge transformation $c_\sigma \to e^{i\chi} c_\sigma$, `bcs[c, Φ]` becomes `bcs[c, Φ - 2χ]`, and a hopping term `hop[c, d]` becomes `hopphi[c, d, -χ]`. The superconducting phase can therefore be moved to the tunneling terms, see `hopphi`.
- `bcs[c[i], Φ]` is `bcs[Exp[I Φ], c[i]]`. Nonlocal pairing between two levels is given by `anhop` (singlet) and `genanhop`; `anhop[c[i], c[i]]` is `2 bcs[c[i]]`.
- `bcs` is not `Listable`, and bare symbols are not accepted as levels.

## Examples
### Basic Examples
The local pairing operator:
```wl
snegfermionoperators[c];
bcs[c[]]
```
With a phase:
```wl
snegrealconstants[phi];
bcs[c[], phi]
```
With a complex amplitude:
```wl
snegcomplexconstants[z];
bcs[z, c[]]
```

### Scope
Pairing on the sites of a Wilson chain or a lattice:
```wl
snegfermionoperators[f];
snegrealconstants[Delta];
-Delta Sum[bcs[f[n]], {n, 0, 2}]
```

### Applications
Bogoliubov quasiparticle of a single level, $H = \xi(n - 1) - \Delta\, \mathrm{bcs}$. The even states $|0\rangle$, $c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$ are mixed, and the excitation energy to the odd (singly occupied) states is $\sqrt{\xi^2 + \Delta^2}$:
```wl
snegfermionoperators[c];
snegrealconstants[xi, Delta];
H = xi (number[c[]] - 1) - Delta bcs[c[]];
heven = matrixrepresentationop[H, {1, nc[c[CR, UP], c[CR, DO]]}]
```
```wl
{Eigenvalues[heven], matrixrepresentationop[H, {c[CR, UP]}]}
```
---
Yu-Shiba-Rusinov (Andreev) state of a quantum dot coupled to a superconductor, in the zero-bandwidth approximation where the superconductor is a single level `f` with the pairing $\Delta$. As the interaction $U$ increases, the ground state changes from a spin singlet (subspace $S_z = 0$) to a spin doublet ($S_z = 1/2$):
```wl
snegfermionoperators[d, f];
snegrealconstants[U, Delta, v];
H = U/2 isozsq[d[]] - Delta bcs[f[]] + v hop[f[], d[]];
mats = makematricesbzvc[H, szbasisvc[{d[], f[]}]];
First /@ mats
```
The lowest energies in the singlet and in the doublet subspace for $\Delta = 1$, $v = 1/2$:
```wl
Table[{u, Min[Eigenvalues[N[mats[[3, 2]] /. {Delta -> 1, v -> 1/2, U -> u}]]], Min[Eigenvalues[N[mats[[4, 2]] /. {Delta -> 1, v -> 1/2, U -> u}]]]}, {u, 0, 4}]
```
---
Phase-biased junction: a dot between two superconducting levels with the phases $\pm\phi/2$. The ground-state energy depends on the phase difference $\phi$; the Josephson current is $2\, \partial E_0/\partial\phi$ (in units of $e/\hbar$):
```wl
snegfermionoperators[d, f];
snegrealconstants[Delta, v, phi];
H = -Delta (bcs[f[1], phi/2] + bcs[f[2], -phi/2]) + v (hop[f[1], d[]] + hop[f[2], d[]]);
mats = makematricesbzvc[H /. {Delta -> 1, v -> 1/2}, szbasisvc[{d[], f[1], f[2]}]];
Table[{p, Min[Flatten[Eigenvalues[N[Last[#] /. phi -> p]] & /@ mats]]}, {p, 0, Pi, Pi/4}]
```

### Properties & Relations
`bcs` is Hermitian and is twice the $x$ component of the isospin:
```wl
snegfermionoperators[c];
snegrealconstants[phi];
{Expand[conj[bcs[c[], phi]] - bcs[c[], phi]], Expand[bcs[c[]] - 2 isospinx[c[]]]}
```
With a phase, $\mathrm{bcs}(c, \Phi) = 2(\cos\Phi\, I^x - \sin\Phi\, I^y)$:
```wl
Expand[ExpToTrig[bcs[c[], phi]] - 2 (Cos[phi] isospinx[c[]] - Sin[phi] isospiny[c[]])]
```
---
Matrix elements in the basis $|0\rangle$, $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[bcs[c[], phi], {1, nc[c[CR, UP], c[CR, DO]]}]
```
---
The pairing term is a spin singlet but changes the charge by $\pm2$:
```wl
{Expand[komutator[bcs[c[]], spinplus[c[]]]], komutator[number[c[]], bcs[c[]]]}
```
---
Gauge transformation $c_\sigma \to e^{i\chi} c_\sigma$:
```wl
snegrealconstants[chi];
gauge = {c[CR, s_] :> Exp[-I chi] c[CR, s], c[AN, s_] :> Exp[I chi] c[AN, s]};
Expand[ExpToTrig[(bcs[c[], phi] /. gauge) - bcs[c[], phi - 2 chi]]]
```
---
Relation with the nonlocal singlet pairing:
```wl
{bcs[c[], phi] == bcs[Exp[I phi], c[]], Expand[anhop[c[], c[]] - 2 bcs[c[]]]}
```

### Possible Issues
The first argument decides the form. With the level first, the second argument is a phase, not an amplitude:
```wl
snegfermionoperators[c];
snegrealconstants[Delta];
{bcs[c[], Delta], bcs[Delta, c[]]}
```
---
Bare symbols are not accepted as levels:
```wl
bcs[c]
```
