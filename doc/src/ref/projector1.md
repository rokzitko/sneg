---
keywords: projector, single occupancy, local moment, Kondo, Schrieffer-Wolff, Heisenberg
see also: projector, PROJ1, projector0, projectorUP, projectorDO, projector2, projector02
---
# projector1

## Usage
- `projector1[c[i]]` gives the projector $P_1 = P_\uparrow + P_\downarrow = n_i - 2 n_{i\uparrow} n_{i\downarrow}$ onto the singly occupied states of the level `c[i]`.
- `projector1[c]` is `projector1[c[]]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes; other levels are not affected.
- `projector1[c[i]]` is `projector[c[i], PROJ1]`. See `projector` for the complete set of local projectors and their relations.
- $P_1$ projects onto the local-moment (spin-1/2) states of the level and commutes with all spin components; $\mathbf S^2 = \frac{3}{4} P_1$.
- Products such as `nc[projector1[c[1]], projector1[c[2]]]` define the low-energy subspace of the half-filled Hubbard model in `schriefferwolff`.
- `projector1` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projector1[c[]]
```
The matrix in the basis $|0\rangle$, $c^\dagger_\uparrow|0\rangle$, $c^\dagger_\downarrow|0\rangle$, $c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projector1[c[]], {1, c[CR, UP], c[CR, DO], nc[c[CR, UP], c[CR, DO]]}]
```

### Scope
`projector1` threads over lists of levels:
```wl
snegfermionoperators[c];
projector1[{c[1], c[2]}] == {projector1[c[1]], projector1[c[2]]}
```

### Applications
Low-energy subspace of the Hubbard dimer for the Schrieffer-Wolff transformation; the second-order effective Hamiltonian is the antiferromagnetic exchange with $J = 4t^2/U$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
P = nc[projector1[c[1]], projector1[c[2]]];
heff = schriefferwolff[U (hubbard[c[1]] + hubbard[c[2]]), -t hop[c[1], c[2]], P, 2];
Simplify[heff[[3]] - 4 t^2/U nc[P, spinspin[c[1], c[2]] - 1/4, P]]
```

### Properties & Relations
$\mathbf S^2 = \frac{3}{4} P_1$, and $P_1$ commutes with the spin operators:
```wl
snegfermionoperators[c];
Expand[{spinss[c[]] - 3/4 projector1[c[]], komutator[projector1[c[]], spinplus[c[]]]}]
```
---
`projector1` is idempotent:
```wl
Expand[nc[projector1[c[]], projector1[c[]]] - projector1[c[]]]
```
