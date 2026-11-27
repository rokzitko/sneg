---
keywords: projected basis, projection, Gutzwiller projection, local moment, Kondo model, spin chain, Heisenberg model, restricted Hilbert space, orthonormalization
see also: transformfunc, projector1, projector0, projector02, qsbasisvc, qszbasisvc, makematricesbzvc, dropemptysubspaces, orthogvc
---
# transformbasis

## Usage
- `transformbasis[bz, op]` applies the operator *op* to all states of the basis *bz* in the occupation number representation, orthonormalizes the resulting states within each invariant subspace, and removes the empty subspaces.

## Details
- *bz* is a basis `{{qn, states}, ...}` (see `makematricesbzvc`) with states in the occupation number representation, e.g. from `qszbasisvc` or `qsbasisvc`. The second argument is an operator expression.
- For each subspace with orthonormal states $|i\rangle$, `transformfunc` computes $\hat O |i\rangle$, expands these vectors in the states $|i\rangle$, and orthonormalizes them (Gram-Schmidt, with vectors proven to vanish dropped). The result is an orthonormal basis of the image $\hat O \mathcal H_{qn}$ of the subspace, with the same quantum numbers.
- The main application is a projector $\hat P$, which restricts the Hilbert space: `projector1[c[i]]` keeps the states with a singly occupied level $i$ (local moment, Kondo and spin models), `projector0`, `projector2`, and `projector02` select empty, doubly occupied, or empty-or-double levels, and products of projectors combine the conditions.
- The operator must conserve the quantum numbers of the basis. The components of $\hat O |i\rangle$ outside the subspace are discarded without warning.
- The calculation uses exact arithmetic and `Simplify` and becomes slow for large subspaces.
- The basis must be in the occupation number representation; convert with `bzop2bzvc`. For a basis in the creation operator representation, the evaluation does not terminate.

## Developer Notes
- `transformbasis` maps `transformfunc` over the subspaces and applies `dropemptysubspaces`; `transformfunc` uses `ap` and `orthogvc`. NRG Ljubljana applies `transformbasis` with the projector given by `BASISRULE` to the initial basis.

## Examples
### Basic Examples
Two-level basis with the first level `d` restricted to single occupancy:
```wl
snegfermionoperators[d, c];
bz = qsbasisvc[{d[], c[]}];
pb = transformbasis[bz, projector1[d[]]]
```
The Kondo exchange $J\, \mathbf S_d \cdot \mathbf s_c$ in the projected basis; the singlet has $-3J/4$, the triplet $J/4$:
```wl
snegrealconstants[J];
makematricesbzvc[J spinspin[d[], c[]], pb]
```

### Scope
The projected subspaces have half the dimension:
```wl
snegfermionoperators[d, c];
bz = qsbasisvc[{d[], c[]}];
{Total[(2 #[[1, 2]] + 1) Length[#[[2]]] & /@ bz], Total[(2 #[[1, 2]] + 1) Length[#[[2]]] & /@ transformbasis[bz, projector1[d[]]]]}
```
---
Projection onto empty or doubly occupied first level in a $(Q, S_z)$ basis:
```wl
snegfermionoperators[c];
transformbasis[qszbasisvc[{c[1], c[2]}], projector02[c[1]]]
```

### Applications
Spin chains from fermion levels: single occupancy on all sites gives the Hilbert space of three spins 1/2. Spectrum of the XXZ chain $H = J \sum_i (S^x_i S^x_{i+1} + S^y_i S^y_{i+1} + \Delta S^z_i S^z_{i+1})$ by total $S_z$:
```wl
snegfermionoperators[c];
snegrealconstants[J, Delta];
bz = transformbasis[qszbasisvc[{c[1], c[2], c[3]}], nc @@ projector1[{c[1], c[2], c[3]}]];
H = J Sum[spinspinxy[c[i], c[i + 1]] + Delta spinspinz[c[i], c[i + 1]], {i, 2}];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, bz]
```
---
Gutzwiller projection: the Hubbard dimer without doubly occupied sites ($U \to \infty$). The singlet and triplet with two electrons are degenerate, the antiferromagnetic exchange $4t^2/U$ is absent:
```wl
snegfermionoperators[c];
snegrealconstants[t];
bz = transformbasis[qsbasisvc[{c[1], c[2]}], nc[1 - projector2[c[1]], 1 - projector2[c[2]]]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[-t hop[c[1], c[2]], bz]
```

### Properties & Relations
The projected states are eigenstates of the projector with eigenvalue 1:
```wl
snegfermionoperators[d, c];
pb = transformbasis[qsbasisvc[{d[], c[]}], projector1[d[]]];
Union[Flatten[makematricesbzvc[projector1[d[]], pb][[All, 2]] - (IdentityMatrix[Length[#]] & /@ pb[[All, 2]])]]
```

### Possible Issues
The operator must not change the quantum numbers. A creation operator maps every subspace to a different one; all components are discarded, and the basis becomes empty:
```wl
snegfermionoperators[d];
transformbasis[qszbasisvc[{d[]}], d[CR, UP]]
```
