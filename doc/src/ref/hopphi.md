---
keywords: Peierls phase, Peierls substitution, magnetic flux, Aharonov-Bohm effect, persistent current, gauge transformation, superconducting phase, Josephson junction, phase-biased junction
see also: hop, genhop, currentphi, current, spinfliphopphi, bcs, projectorEphi, matrixhop
related links: [L. Pavešić, R. Aguado, and R. Žitko, Phys. Rev. B 109, 125131 (2024) (quantum-dot Josephson junctions)](https://doi.org/10.1103/PhysRevB.109.125131); [J. Vučičević and R. Žitko, Phys. Rev. B 104, 205101 (2021) (conductivity of the Hubbard model in a magnetic field)](https://doi.org/10.1103/PhysRevB.104.205101)
---
# hopphi

## Usage
- `hopphi[a[i], b[j], φ]` gives the hopping operator with the phase *φ*, $\sum_\sigma (e^{i\phi} a^\dagger_{i\sigma} b_{j\sigma} + e^{-i\phi} b^\dagger_{j\sigma} a_{i\sigma})$.
- `hopphi[a[i], b[j], σ, φ]` gives $e^{i\phi} a^\dagger_{i\sigma} b_{j\sigma} + e^{-i\phi} b^\dagger_{j\sigma} a_{i\sigma}$ for the spin projection *σ* only.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes, as in `hop`. The form without *σ* requires spin-1/2 fermions.
- *φ* is the phase acquired by an electron that hops from `b[j]` to `a[i]`. `hopphi[a, b, φ]` equals `hopphi[b, a, -φ]` and `genhop[Exp[I φ], a, b]`; `hopphi[a, b, 0]` is `hop[a, b]`.
- Peierls substitution: in a vector potential, the hopping $-t\, c_i^\dagger c_j$ acquires the phase $\phi_{ij} = (e/\hbar)\int_j^i \mathbf A\cdot d\mathbf l$ (up to the sign convention for the charge), $H = -t \sum_{\langle ij\rangle}$ `hopphi[c[i], c[j], φij]`. The sum of the phases around a closed loop is $2\pi\Phi/\Phi_0$, with $\Phi$ the magnetic flux through the loop and $\Phi_0 = h/e$.
- Gauge transformation: the phase can be moved from the hopping to the level, $a_\sigma \to e^{i\phi} a_\sigma$ turns `hopphi[a, b, φ]` into `hop[a, b]`. A pairing term on that level then acquires a phase: `bcs[a, Φ]` becomes `bcs[a, Φ - 2φ]`. Hence a superconductor with the phase $\Phi$ coupled by `hop` is equivalent to a superconductor with real pairing coupled by `hopphi` with $\phi = -\Phi/2$; this is the form used for phase-biased junctions in NRG Ljubljana, where the superconducting phases are put on the tunneling terms.
- The particle current from `b[j]` to `a[i]` for $H = $ `hopphi[a[i], b[j], φ]` is $i[H, n_a]$ = `currentphi[a[i], b[j], φ]`; equivalently, $\partial H/\partial\phi = -$`currentphi[a[i], b[j], φ]`.
- `hopphi` is not `Listable`.

## Examples
### Basic Examples
Hopping with a phase:
```wl
snegfermionoperators[a, b];
snegrealconstants[phi];
hopphi[a[], b[], phi]
```
Spin-up part only:
```wl
hopphi[a[], b[], UP, phi]
```

### Scope
A phase $\pi/2$ gives the purely imaginary hopping $i(a^\dagger b - b^\dagger a)$:
```wl
snegfermionoperators[a, b];
hopphi[a[], b[], Pi/2]
```

### Applications
Ring of $N = 4$ sites threaded by the flux $\phi$ (in units of $\Phi_0/2\pi$), with the phase $\phi/N$ on each bond. The levels are $-2t\cos(2\pi m/N + \phi/N)$:
```wl
snegfermionoperators[c];
snegrealconstants[t, phi];
H = -t Sum[hopphi[c[j], c[Mod[j, 4] + 1], phi/4], {j, 4}];
h = matrixrepresentationop[H, Table[c[CR, j, UP], {j, 4}]];
FullSimplify[ExpToTrig[Eigenvalues[h]]]
```
The spectrum is periodic in the flux with period $2\pi$ (one flux quantum); the levels are permuted:
```wl
Sort[N[Eigenvalues[h /. {t -> 1, phi -> 0.3}]]] == Sort[N[Eigenvalues[h /. {t -> 1, phi -> 0.3 + 2 Pi}]]]
```
---
Gauge equivalence in a phase-biased superconducting junction: a quantum dot `d` coupled to two superconducting levels `f[1]`, `f[2]` (zero-bandwidth model) with the phases $\pm\phi/2$ in the pairing terms has the same spectrum as the model with real pairing and the phases $\mp\phi/4$ on the tunneling terms:
```wl
snegfermionoperators[d, f];
snegrealconstants[Delta, v, U, phi];
ops = {d[], f[1], f[2]};
H1 = -Delta (bcs[f[1], phi/2] + bcs[f[2], -phi/2]) + v (hop[f[1], d[]] + hop[f[2], d[]]) + U/2 isozsq[d[]];
H2 = -Delta (bcs[f[1]] + bcs[f[2]]) + v (hopphi[f[1], d[], -phi/4] + hopphi[f[2], d[], phi/4]) + U/2 isozsq[d[]];
bz = szbasisvc[ops];
spectrum[H_] := Sort[Flatten[Eigenvalues[N[Last[#] /. {Delta -> 1, v -> 0.5, U -> 2, phi -> 0.7}]] & /@ makematricesbzvc[H, bz]]];
Max[Abs[spectrum[H1] - spectrum[H2]]] < 10^-10
```

### Properties & Relations
The phase can be removed by the gauge transformation $a_\sigma \to e^{i\phi} a_\sigma$:
```wl
snegfermionoperators[a, b];
snegrealconstants[phi];
gauge = {a[CR, s_] :> Exp[-I phi] a[CR, s], a[AN, s_] :> Exp[I phi] a[AN, s]};
Expand[(hopphi[a[], b[], phi] /. gauge) - hop[a[], b[]]]
```
The same transformation changes the phase of a pairing term on `a` by $-2\phi$:
```wl
Expand[(bcs[a[]] /. gauge) - bcs[a[], -2 phi]]
```
---
Relation with the current, $i[H, n_a] = -\partial H/\partial\phi$ = `currentphi`:
```wl
H = hopphi[a[], b[], phi];
Expand[ExpToTrig[{I komutator[H, number[a[]]] - currentphi[a[], b[], phi], D[H, phi] + currentphi[a[], b[], phi]}]]
```
---
Exchanging the levels reverses the phase:
```wl
{hopphi[a[], b[], phi] == hopphi[b[], a[], -phi], hopphi[a[], b[], 0] == hop[a[], b[]]}
```
