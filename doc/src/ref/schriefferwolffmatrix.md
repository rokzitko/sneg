---
keywords: Schrieffer-Wolff transformation, effective Hamiltonian, matrix perturbation theory, canonical transformation, block diagonalization, quasi-degenerate perturbation theory, superexchange, Lambda system
see also: schriefferwolff, eigenoperators, makematricesbzvc, qszbasisvc
related links: [S. Bravyi, D. P. DiVincenzo, and D. Loss, Ann. Phys. 326, 2793 (2011)](https://doi.org/10.1016/j.aop.2011.06.004); [Á. Bácsi, T. Iličin, and R. Žitko, Phys. Rev. B 113, 064501 (2026) (Schrieffer-Wolff approach to Josephson junctions)](https://doi.org/10.1103/jfcl-7pzt)
---
# schriefferwolffmatrix

## Usage
- `schriefferwolffmatrix[H0, V, pind, n]` performs the canonical Schrieffer-Wolff transformation of the matrix $H_0 + V$ to order *n* in *V*, with the low-energy subspace $P$ spanned by the basis states with indexes *pind*, and returns the list $\{P H_0 P, P V P, P H^{(2)}_\mathrm{eff} P, \ldots, P H^{(n)}_\mathrm{eff} P\}$ of matrices.

## Details
- The transformation is the same as in `schriefferwolff`: $H_\mathrm{eff} = e^S (H_0 + V) e^{-S}$ with the anti-Hermitian, block-off-diagonal generator $S = S_1 + S_2 + \cdots$ ($P S P = Q S Q = 0$, $S_m$ of order $V^m$). Order by order, $[H_0, S_m] = P R_m Q + Q R_m P$ and $H^{(m)}_\mathrm{eff} = P R_m P$, where $R_m$ is the part of order $m$ of $e^{S'} H e^{-S'}$, $S' = S_1 + \cdots + S_{m-1}$.
- At second order, $H^{(2)}_\mathrm{eff} = \frac12 P [S_1, V] P$, with the matrix elements
$$(H^{(2)}_\mathrm{eff})_{ij} = \frac12 \sum_{q \in Q} V_{iq} V_{qj} (\frac{1}{E_i - E_q} + \frac{1}{E_j - E_q}),\qquad i, j \in P,$$
in the eigenbasis of $H_0$. For a degenerate $P$ subspace with energy $E_P$ this is $P V Q (E_P - H_0)^{-1} Q V P$. The $P$ subspace need not be degenerate; it only has to be separated from $Q$ (no vanishing denominators $E_i - E_q$).
- *H0* and *V* are $d \times d$ matrices, and *pind* is a list of distinct indexes in $1, \ldots, d$; $Q$ is spanned by the other basis states. The results are $|P| \times |P|$ matrices in the basis given by *pind* (in that order).
- *H0* must not couple $P$ and $Q$ (`schriefferwolffmatrix::block`). If *H0* is diagonal, it may be symbolic. A non-diagonal *H0* must be numerical; it is diagonalized within the $P$ and $Q$ blocks, and the results are transformed back to the original basis (`schriefferwolffmatrix::diag` for symbolic non-diagonal matrices).
- A nonzero element of a block-off-diagonal remainder with a vanishing energy denominator gives the message `schriefferwolff::res` and `$Failed`.
- Invalid dimensions or indexes give `schriefferwolffmatrix::dim` and `$Failed`.
- The following option can be given:

| `"Generator"` | `False` | whether to also return the generators $S_1, \ldots, S_n$ ($d \times d$ matrices) |

- With `"Generator" -> True`, the result is `{list, {S1, ..., Sn}}`.
- Symbolic matrix elements are combined with `Together`. The cost grows with the order and the dimension; matrices with tens of states and orders up to about six are practical.
- `schriefferwolffmatrix` is useful when $H_0$ is not diagonal in the occupation numbers, as required by `schriefferwolff`, for numerical studies, and as an independent check of operator results.

## Examples
### Basic Examples
A two-level system with the splitting $\Delta$ and the coupling $v$. The effective energy of the lower level reproduces the expansion of $(\Delta - \sqrt{\Delta^2 + 4 v^2})/2$:
```wl
snegrealconstants[D1, v];
schriefferwolffmatrix[{{0, 0}, {0, D1}}, {{0, v}, {v, 0}}, {1}, 6]
```
```wl
Series[(D1 - Sqrt[D1^2 + 4 v^2])/2, {v, 0, 6}, Assumptions -> D1 > 0]
```

### Scope
#### Degenerate subspaces
A $\Lambda$ system: two degenerate states coupled with the same amplitude $v$ to an excited state at energy $\Delta$. At second order, the symmetric combination is shifted by $-2v^2/\Delta$, while the antisymmetric (dark) state remains at zero energy:
```wl
snegrealconstants[D1, v];
heff = schriefferwolffmatrix[{{0, 0, 0}, {0, 0, 0}, {0, 0, D1}}, {{0, 0, v}, {0, 0, v}, {v, v, 0}}, {1, 2}, 2];
{heff[[3]], Eigenvalues[heff[[3]]]}
```
#### Non-degenerate subspaces
Two states at energies $0$ and $\delta$ coupled to an excited state at $\Delta$; the second-order element between them contains the average of the two energy denominators:
```wl
snegrealconstants[dl, D1, v1, v2];
schriefferwolffmatrix[{{0, 0, 0}, {0, dl, 0}, {0, 0, D1}}, {{0, 0, v1}, {0, 0, v2}, {v1, v2, 0}}, {1, 2}, 2][[3]] // Simplify
```
#### Non-diagonal H0
A numerical $H_0$ that is block-diagonal but not diagonal is diagonalized within the blocks; the result is the same as in a rotated basis:
```wl
h0m = {{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1.5, 0}, {0, 0, 0, 3}};
vm = {{0, 0.2, 0.1, 0}, {0.2, 0, 0, 0.3}, {0.1, 0, 0, 0.2}, {0, 0.3, 0.2, 0}};
w = ArrayFlatten[{{1, 0, 0}, {0, RotationMatrix[0.4], 0}, {0, 0, 1}}];
r1 = schriefferwolffmatrix[h0m, vm, {1, 4}, 3];
r2 = schriefferwolffmatrix[w . h0m . Transpose[w], w . vm . Transpose[w], {1, 4}, 3];
Chop[r1 - r2]
```

### Options
#### Generator
The first-order generator of the two-level system, $S_1 = (v/\Delta)(|2\rangle\langle 1| - |1\rangle\langle 2|)$, is anti-Hermitian:
```wl
snegrealconstants[D1, v];
{heff, gen} = schriefferwolffmatrix[{{0, 0}, {0, D1}}, {{0, v}, {v, 0}}, {1}, 2, "Generator" -> True];
gen
```

### Applications
#### Superexchange in the Hubbard dimer
The matrices of the Hubbard dimer in the subspace with two electrons and $S_z = 0$, with the basis states from `qszbasisvc`; $H_0 = U (n_{1\uparrow} n_{1\downarrow} + n_{2\uparrow} n_{2\downarrow})$ is diagonal and $V$ is the hopping:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
bas = qszbasisvc[{c[1], c[2]}];
h0m = Last[First[Select[makematricesbzvc[U (hubbard[c[1]] + hubbard[c[2]]), bas], First[#] == {0, 0} &]]];
vm = Last[First[Select[makematricesbzvc[-t hop[c[1], c[2]], bas], First[#] == {0, 0} &]]];
{MatrixForm[h0m], MatrixForm[vm]}
```
The low-energy subspace consists of the two states with singly occupied sites. The effective Hamiltonian to fourth order gives the singlet-triplet splitting $4t^2/U - 16 t^4/U^3$:
```wl
pind = Flatten[Position[Diagonal[h0m], 0]];
heff = schriefferwolffmatrix[h0m, vm, pind, 4];
Eigenvalues[Total[heff]]
```
The exact singlet energy is $(U - \sqrt{U^2 + 16 t^2})/2$:
```wl
Series[(U - Sqrt[U^2 + 16 t^2])/2, {t, 0, 4}, Assumptions -> U > 0]
```

### Properties & Relations
The transformation removes the coupling between $P$ and $Q$ up to terms of order $V^{n+1}$. For the two-level system at fourth order, the block-off-diagonal element of $e^S H e^{-S}$ decreases as $\lambda^5$ when $V$ is scaled by $\lambda$:
```wl
h0m = {{0, 0}, {0, 1.}}; vm = {{0, 1.}, {1., 0}};
{heff, gen} = schriefferwolffmatrix[h0m, vm, {1}, 4, "Generator" -> True];
off[lam_] := Abs[(MatrixExp[Sum[lam^m gen[[m]], {m, 4}]] . (h0m + lam vm) . MatrixExp[-Sum[lam^m gen[[m]], {m, 4}]])[[1, 2]]];
off[1/10]/off[1/20]
```
---
`schriefferwolff` gives the same result for operators; `schriefferwolffmatrix` serves as an independent check, see the Applications above and the examples of `schriefferwolff`.

### Possible Issues
$P$ and $Q$ must be separated in energy; a coupling between degenerate states of $P$ and $Q$ is a resonance:
```wl
schriefferwolffmatrix[{{0, 0}, {0, 0}}, {{0, 1}, {1, 0}}, {1}, 2]
```
---
$H_0$ must not couple $P$ and $Q$:
```wl
schriefferwolffmatrix[{{0, 1}, {1, 2}}, {{0, 0}, {0, 0}}, {1}, 2]
```
---
A symbolic $H_0$ must be diagonal:
```wl
snegrealconstants[t, U];
schriefferwolffmatrix[{{U, 0, 0}, {0, 0, t}, {0, t, 0}}, IdentityMatrix[3], {1}, 2]
```
