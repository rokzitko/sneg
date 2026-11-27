---
keywords: SU(2) coupling, Clebsch-Gordan coefficients, angular momentum addition, total spin, tensor product, highest-weight states, Kondo model, impurity spin, NRG
see also: qsbasis, basistensorproduct, spinbasis, spindown, fixspin, fixspinket, qscombinefnc, qsspincombinefnc, ClebschGordan
---
# SU2basistensorproduct

## Usage
- `SU2basistensorproduct[bz1, bz2, s1, s2, qn, fix1, fix2]` couples the SU(2) multiplets of the bases *bz1* and *bz2* to multiplets of the total spin with Clebsch-Gordan coefficients, and gives the basis of the highest-weight states of the coupled multiplets.

## Details
- *bz1* and *bz2* are bases `{{qn, states}, ...}` in the creation operator representation whose states are highest-weight states, $|S, S_z = S\rangle$, e.g. from `qsbasis` or `{First[spinbasis[S]]}` (spin kets).
- The functions in the arguments:

| *s1*, *s2* | give the spin of a subspace from its quantum numbers, e.g. `Last` for $(Q, S)$, `First` for $(S)$ |
| *qn* | `qn[S, qn1, qn2]` gives the quantum numbers of the coupled states with total spin $S$; `qscombinefnc` gives $(Q_1 + Q_2, S)$, `qsspincombinefnc` gives $(Q_1, S)$ |
| *fix1*, *fix2* | `fix[state, s, m]` gives the member with $S_z = m$ of the multiplet of the highest-weight *state* of spin *s*; `fixspin` lowers operator strings with `spindown`, `fixspinket` lowers spin kets `ket[m]` |

- For each pair of highest-weight states $|S_1 S_1\rangle$ and $|S_2 S_2\rangle$, the coupled highest-weight states are
$$|S, S\rangle = \sum_{m_1 + m_2 = S} \langle S_1 m_1; S_2 m_2 | S\, S\rangle\, |S_1 m_1\rangle |S_2 m_2\rangle, \quad S = |S_1 - S_2|, \ldots, S_1 + S_2,$$
with the coefficients of `ClebschGordan` (Condon-Shortley convention). The product $|S_1 m_1\rangle |S_2 m_2\rangle$ is `nc[fix1[state1, S1, m1], fix2[state2, S2, m2]]`.
- The result is merged with `mergebasis`. Each state represents a multiplet of $2S+1$ states.
- The phases of the coupled states follow from the Clebsch-Gordan coefficients and from the order of the factors; they can differ from those of `qsbasis` for the same system.
- `SU2basistensorproduct` does not call `makebasis`.

## Developer Notes
- The coupling of a pair of subspaces is performed by the internal function `SU2merge`, and of a pair of states by `SU2combine`.
- NRG Ljubljana uses `SU2basistensorproduct` to add an impurity spin (`MAKESPINKET`) or an orbital moment (`MAKEORBKET`) to the fermionic $(Q, S)$ basis, with model-specific combination functions.

## Examples
### Basic Examples
Coupling the $(Q, S)$ bases of two levels gives the $(Q, S)$ basis of the dimer:
```wl
snegfermionoperators[c];
bz = SU2basistensorproduct[qsbasis[{c[1]}], qsbasis[{c[2]}], Last, Last, qscombinefnc, fixspin, fixspin]
```
The dimensions agree with `qsbasis`:
```wl
{First[#], Length[Last[#]]} & /@ bz === ({First[#], Length[Last[#]]} & /@ qsbasis[{c[1], c[2]}])
```

### Applications
Kondo model with one conduction level: a conduction electron level $c$ coupled to an impurity spin 1/2, represented by kets, $H = J\, \mathbf s_c \cdot \mathbf S$. The basis states have well-defined charge $Q$ of the conduction level and total spin $S$:
```wl
snegfermionoperators[c];
snegrealconstants[J];
bz = SU2basistensorproduct[qsbasis[{c[]}], {First[spinbasis[1/2]]}, Last, First, qsspincombinefnc, fixspin, fixspinket]
```
With one electron, the singlet has the energy $-3J/4$ and the triplet $J/4$:
```wl
makebasis[{c[]}];
H = J Expand[spinxyz[c[]] ~inner~ spinketbraXYZ[1/2]];
makematricesbzvc[H, bzop2bzvc[bz]]
```
---
Impurity spin 1 coupled to a conduction level (underscreened Kondo model):
```wl
snegfermionoperators[c];
snegrealconstants[J];
bz = SU2basistensorproduct[qsbasis[{c[]}], {First[spinbasis[1]]}, Last, First, qsspincombinefnc, fixspin, fixspinket];
makebasis[{c[]}];
H = J Expand[spinxyz[c[]] ~inner~ spinketbraXYZ[1]];
makematricesbzvc[H, bzop2bzvc[bz]]
```

### Properties & Relations
The coupled states are eigenstates of the total spin $\mathbf S_\mathrm{tot} = \mathbf s_c + \mathbf S$ with $\mathbf S_\mathrm{tot}^2 = S(S+1)$:
```wl
snegfermionoperators[c];
bz = SU2basistensorproduct[qsbasis[{c[]}], {First[spinbasis[1/2]]}, Last, First, qsspincombinefnc, fixspin, fixspinket];
makebasis[{c[]}];
stot = spinxyz[c[]] + spinketbraXYZ[1/2];
s2 = Expand[stot ~inner~ stot];
makematricesbzvc[s2, bzop2bzvc[bz]]
```
