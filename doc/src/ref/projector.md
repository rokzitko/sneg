---
keywords: projector, projection operator, occupancy, empty, singly occupied, doubly occupied, local moment, Gutzwiller projection, double occupancy
see also: projector0, projector1, projector2, projector02, projectorUP, projectorDO, PROJ0, PROJ1, PROJ2, PROJ02, PROJUP, PROJDO, number, hubbard, projectorEphi, schriefferwolff
---
# projector

## Usage
- `projector[c[i], p]` gives the projector onto the subspace of the level `c[i]` specified by *p*, one of `PROJ0`, `PROJUP`, `PROJDO`, `PROJ2`, `PROJ1`, `PROJ02`.
- `projector[c, p]` is `projector[c[], p]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes. The projectors act on the four states $|0\rangle$, $|{\uparrow}\rangle$, $|{\downarrow}\rangle$, $|2\rangle$ of the level, and as the identity on the other levels.
- The projectors, written with $n_\sigma = c^\dagger_\sigma c_\sigma$:

| `PROJ0` | `projector0` | $P_0 = (1 - n_\uparrow)(1 - n_\downarrow)$, empty |
| `PROJUP` | `projectorUP` | $P_\uparrow = n_\uparrow (1 - n_\downarrow)$, one spin-up electron |
| `PROJDO` | `projectorDO` | $P_\downarrow = n_\downarrow (1 - n_\uparrow)$, one spin-down electron |
| `PROJ2` | `projector2` | $P_2 = n_\uparrow n_\downarrow$, doubly occupied |
| `PROJ1` | `projector1` | $P_1 = P_\uparrow + P_\downarrow = n - 2 n_\uparrow n_\downarrow$, singly occupied |
| `PROJ02` | `projector02` | $P_{02} = P_0 + P_2 = (n - 1)^2$, empty or doubly occupied |

- `PROJ0`, ..., `PROJ02` are the integers 1, ..., 6; `projector[c[i], k]` with these integers gives the same results. Other values of *p* leave `projector` unevaluated.
- The projectors are diagonal in the occupation numbers, commute with the number and $S^z$ operators of the level, and satisfy $P_a P_b = \delta_{ab} P_a$ and $P_0 + P_\uparrow + P_\downarrow + P_2 = 1$.
- Products of projectors on several levels project onto configurations of several sites, e.g. `nc[projector1[c[1]], projector1[c[2]]]` onto the subspace with single occupancy of both sites. They are used to define the low-energy subspace in `schriefferwolff`.
- For projectors onto the superpositions $(|0\rangle \pm e^{-i\phi}|2\rangle)/\sqrt2$, see `projectorEphi` and `projectorOphi`.
- `projector` and the short forms `projector0`, ... are `Listable`, also in the second argument.

## Examples
### Basic Examples
The projector onto the singly occupied states:
```wl
snegfermionoperators[c];
projector[c[], PROJ1]
```
It equals `projector1`:
```wl
projector[c[], PROJ1] == projector1[c[]]
```

### Scope
All projectors of a level:
```wl
snegfermionoperators[c];
Thread[{"PROJ0", "PROJUP", "PROJDO", "PROJ2", "PROJ1", "PROJ02"} -> projector[c[1], {PROJ0, PROJUP, PROJDO, PROJ2, PROJ1, PROJ02}]]
```
---
A bare symbol is a level without indexes:
```wl
projector[c, PROJ2] === projector[c[], PROJ2]
```
---
`projector` threads over lists of levels:
```wl
Total[projector[{c[1], c[2]}, PROJ2]]
```

### Applications
Action on the basis states of a level:
```wl
snegfermionoperators[c];
states = {VACUUM, nc[c[CR, UP], VACUUM], nc[c[CR, DO], VACUUM], nc[c[CR, UP], c[CR, DO], VACUUM]};
Table[nc[projector[c[], p], #] & /@ states, {p, {PROJ0, PROJUP, PROJDO, PROJ2}}]
```
---
Double occupancy in the ground state of the half-filled Hubbard dimer. The matrix of $D = \sum_i P_2(i)$ in the subspace $Q = 0$, $S_z = 0$ is used with the ground-state vector; the result is $\frac{1}{2}(1 - U/\sqrt{U^2 + 16t^2})$ for both sites together:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
bz = Select[qszbasisvc[{c[1], c[2]}], First[#] == {0, 0} &];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
{hmat} = Last /@ makematricesbzvc[H /. {t -> 1, U -> 4}, bz];
{dmat} = Last /@ makematricesbzvc[projector2[c[1]] + projector2[c[2]], bz];
{vals, vecs} = Eigensystem[N[hmat]];
psi = vecs[[First[Ordering[vals, 1]]]];
{Conjugate[psi].dmat.psi, N[(1 - 4/Sqrt[4^2 + 16])/2]}
```
---
The hybridization of an impurity level `d` with a bath level `f` changes the charge of the impurity, so that its projection onto the local-moment subspace vanishes, $P_1 V P_1 = 0$; the effective coupling arises in second order (Schrieffer-Wolff transformation):
```wl
snegfermionoperators[d, f];
Expand[nc[projector1[d[]], hop[d[], f[]], projector1[d[]]]]
```
---
The square of the spin of a level is $\mathbf S^2 = \frac{3}{4} P_1$:
```wl
Expand[spinss[d[]] - 3/4 projector1[d[]]]
```

### Properties & Relations
Idempotence, orthogonality, and completeness:
```wl
snegfermionoperators[c];
P = projector[c[], #] & /@ {PROJ0, PROJUP, PROJDO, PROJ2};
{Table[Expand[nc[P[[a]], P[[b]]]] == If[a == b, P[[a]], 0], {a, 4}, {b, 4}], Expand[Total[P]]}
```
---
Relations between the projectors and with the number and spin operators:
```wl
Expand[{projector1[c[]] - projectorUP[c[]] - projectorDO[c[]], projector02[c[]] - projector0[c[]] - projector2[c[]], projectorUP[c[]] - projectorDO[c[]] - 2 spinz[c[]], projector1[c[]] + 2 projector2[c[]] - number[c[]]}]
```
---
The projectors commute with the number and $S^z$ operators of the level:
```wl
Union[Flatten[Expand[{komutator[#, number[c[]]], komutator[#, spinz[c[]]]}] & /@ P]]
```
---
`projector1` and `projector02` commute with all components of the spin:
```wl
Expand[{komutator[projector1[c[]], spinplus[c[]]], komutator[projector02[c[]], spinplus[c[]]], komutator[projectorUP[c[]], spinplus[c[]]]}]
```

### Possible Issues
Only the six values `PROJ0`, ..., `PROJ02` are defined:
```wl
snegfermionoperators[c];
projector[c[], 7]
```
---
The argument is the level, not an operator: `projector[c[CR, UP], PROJ2]` treats `CR` and `UP` as level indexes:
```wl
projector[c[CR, UP], PROJ2]
```
