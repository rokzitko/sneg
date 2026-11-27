---
keywords: current operator, Peierls phase, Josephson current, supercurrent, persistent current, phase-biased junction, tunneling current
see also: current, hopphi, hop, genhop, bcs, number
related links: [L. Pavešić, R. Aguado, and R. Žitko, Phys. Rev. B 109, 125131 (2024) (quantum-dot Josephson junctions)](https://doi.org/10.1103/PhysRevB.109.125131)
---
# currentphi

## Usage
- `currentphi[a[i], b[j], φ]` gives the particle-current operator $\sum_\sigma (-i e^{i\phi} a^\dagger_{i\sigma} b_{j\sigma} + i e^{-i\phi} b^\dagger_{j\sigma} a_{i\sigma})$ associated with `hopphi[a[i], b[j], φ]`.
- `currentphi[a[i], b[j], σ, φ]` gives the current of the spin projection *σ* only.

## Details
- `currentphi[a[i], b[j], φ]` is $i[\mathrm{hopphi}(a_i, b_j, \phi), n_a]$ with $n_a$ = `number[a[i]]`, the particle current from `b[j]` to `a[i]` for the tunneling term $+$`hopphi[a[i], b[j], φ]`. It equals `hopphi[a[i], b[j], φ - Pi/2]` and $-\partial\, \mathrm{hopphi}/\partial\phi$.
- For the tunneling term $v$ `hopphi[a[i], b[j], φ]`, the current from `b[j]` to `a[i]` is $v$ `currentphi[a[i], b[j], φ]`.
- `currentphi[a[i], b[j], 0]` is `current[a[i], b[j]]`.
- Josephson current: if the superconducting phase difference enters the Hamiltonian only through tunneling phases (see `hopphi`), the supercurrent operator follows from $\partial H/\partial\phi$, and the equilibrium current is $I = (2e/\hbar)\, \partial F/\partial\phi$, where $\phi$ is the phase difference. For phases $\pm\phi/4$ on the two tunneling terms, $\partial H/\partial\phi$ is a combination of `currentphi` operators with the factors $\mp1/4$.
- The result contains the phase factors in the form `Exp[I (φ - Pi/2)]`; apply `ExpToTrig` or `Simplify` to simplify them.
- The form without *σ* requires spin-1/2 fermions. `currentphi` is not `Listable`.

## Examples
### Basic Examples
Current operator for tunneling with a phase:
```wl
snegfermionoperators[a, b];
snegrealconstants[phi];
ExpToTrig[currentphi[a[], b[], phi]]
```
It is the time derivative of $n_a$ for $H = $ `hopphi[a[], b[], φ]`:
```wl
Expand[ExpToTrig[I komutator[hopphi[a[], b[], phi], number[a[]]] - currentphi[a[], b[], phi]]]
```

### Scope
Spin-resolved current:
```wl
snegfermionoperators[a, b];
snegrealconstants[phi];
ExpToTrig[currentphi[a[], b[], UP, phi]]
```

### Applications
Supercurrent through a quantum dot between two superconducting levels with the phases on the tunneling terms, $H_\mathrm{T} = v\,[\mathrm{hopphi}(f_1, d, -\phi/4) + \mathrm{hopphi}(f_2, d, \phi/4)]$. The operator $\partial H/\partial\phi$ is a combination of the currents into the two levels:
```wl
snegfermionoperators[d, f];
snegrealconstants[v, phi];
HT = v (hopphi[f[1], d[], -phi/4] + hopphi[f[2], d[], phi/4]);
Expand[ExpToTrig[D[HT, phi] - v/4 (currentphi[f[1], d[], -phi/4] - currentphi[f[2], d[], phi/4])]]
```
The ground-state energy of the zero-bandwidth model at $U = 0$ depends on the phase difference; the supercurrent is $I = 2\, \partial E_0/\partial\phi$ (in units of $e/\hbar$):
```wl
snegrealconstants[Delta];
H = -Delta (bcs[f[1]] + bcs[f[2]]) + HT;
bz = szbasisvc[{d[], f[1], f[2]}];
mats = makematricesbzvc[H /. {Delta -> 1, v -> 0.5}, bz];
E0[p_?NumericQ] := Min[Flatten[Eigenvalues[N[Last[#] /. phi -> p]] & /@ mats]];
Table[{p, E0[p]}, {p, 0, Pi, Pi/4}]
```

### Properties & Relations
`currentphi` is `hopphi` with the phase shifted by $-\pi/2$, and the derivative of `hopphi` with respect to the phase:
```wl
snegfermionoperators[a, b];
snegrealconstants[phi];
Expand[ExpToTrig[{currentphi[a[], b[], phi] - hopphi[a[], b[], phi - Pi/2], D[hopphi[a[], b[], phi], phi] + currentphi[a[], b[], phi]}]]
```
---
At zero phase it is `current`:
```wl
currentphi[a[], b[], 0] == current[a[], b[]]
```
