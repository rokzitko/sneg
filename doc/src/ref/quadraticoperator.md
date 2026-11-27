---
keywords: quadratic Hamiltonian, BdG matrix, Nambu spinor, hopping matrix, operator from matrix, single-particle Hamiltonian
see also: quadraticform, bogoliubov, bogoliubovrules, matrixhop, bdghamiltonian
---
# quadraticoperator

## Usage
- `quadraticoperator[M, {a1, ..., an}, E0]` gives the operator $\frac12 \Psi^\dagger M \Psi + E_0$ for a $2n \times 2n$ Bogoliubov-de Gennes matrix *M*, with $\Psi = (a_1, \ldots, a_n, a_1^\dagger, \ldots, a_n^\dagger)^T$.
- `quadraticoperator[h, {a1, ..., an}, E0]` gives $\sum_{ij} a_i^\dagger h_{ij} a_j + E_0$ for an $n \times n$ matrix *h*.
- `quadraticoperator[M, modes]` is `quadraticoperator[M, modes, 0]`.

## Details
- `quadraticoperator` is the inverse of `quadraticform`: with `{M, E0} = quadraticform[H, modes]`, `quadraticoperator[M, modes, E0]` reconstructs $H$; the same holds for the hopping form returned with `"BdG" -> False`.
- The form is chosen by the dimension of the matrix: $2n \times 2n$ gives the BdG form, $n \times n$ the hopping form. Other dimensions give the message `quadraticoperator::dim` and `$Failed`.
- The modes are specified as in `quadraticform`: `c` or `c[j...]` (for fermions with spin, the spin index is added as the last index and varies fastest; spinless fermions get the index `0`), or `Function[t, expr]` giving the operator for `t = CR, AN`.
- In the BdG form, $\frac12 \Psi^\dagger M \Psi = \frac12 \sum_{pq} M_{pq} \Psi_p^\dagger \Psi_q$ is brought to normal order by `nc`. For a fermionic BdG matrix with the blocks $h$, $\Delta$, $\Delta^\dagger$, $-h^T$ this gives $\sum_{ij} h_{ij} a_i^\dagger a_j + \frac12 \sum_{ij} (\Delta_{ij} a_i^\dagger a_j^\dagger + \mathrm{h.c.}) - \frac12 \mathrm{Tr}\, h$; for a bosonic matrix with the blocks $A$, $B$, $B^*$, $A^*$, the constant is $+\frac12 \mathrm{Tr}\, A$.
- The matrix is not checked for Hermiticity or for the BdG structure. A matrix without the BdG symmetry gives the operator $\frac12 \Psi^\dagger M \Psi$ as written; the symmetrized matrix is returned by `quadraticform`.
- The result is expanded.

## Examples
### Basic Examples
A two-site hopping problem from its $2 \times 2$ matrix:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, eps];
quadraticoperator[{{eps, -t}, {-t, eps}}, {f[1], f[2]}]
```
---
The BdG form of on-site singlet pairing; the constant $E_0 = \xi$ cancels the constant produced by normal ordering:
```wl
snegfermionoperators[c];
snegrealconstants[xi, Dl];
quadraticoperator[{{xi, 0, 0, -Dl}, {0, xi, Dl, 0}, {0, Dl, -xi, 0}, {-Dl, 0, 0, -xi}}, {c[1]}, xi]
```

### Scope
Bosonic BdG matrices give squeezing terms. The constant $\frac12 \mathrm{Tr}\, A$ from normal ordering is compensated by $E_0 = -A/2$:
```wl
snegbosonoperators[b];
snegrealconstants[A, B];
quadraticoperator[{{A, B}, {B, A}}, {b}, -A/2]
```
---
Modes given as functions; the hopping matrix in the basis of bonding and antibonding orbitals gives the hopping in the site basis:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
bond = Function[x, (f[x, 1, 0] + f[x, 2, 0])/Sqrt[2]];
anti = Function[x, (f[x, 1, 0] - f[x, 2, 0])/Sqrt[2]];
Expand[quadraticoperator[{{-t, 0}, {0, t}}, {bond, anti}] + t hop[f[1], f[2]]]
```

### Properties & Relations
Round trip with `quadraticform` for a general BdG Hamiltonian of two spinless fermions:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[e1, e2, t, Dl];
H = e1 number[f[1]] + e2 number[f[2]] - t hop[f[1], f[2]] + Dl (nc[f[CR, 1, 0], f[CR, 2, 0]] + nc[f[AN, 2, 0], f[AN, 1, 0]]) + 1;
{M, E0} = quadraticform[H, {f[1], f[2]}];
Expand[quadraticoperator[M, {f[1], f[2]}, E0] - H]
```
---
The ground-state energy of a diagonal BdG Hamiltonian is $E_0 - \frac12 \sum_m \varepsilon_m$ for fermions: $\frac12 \Psi^\dagger \mathrm{diag}(\varepsilon, -\varepsilon) \Psi = \sum_m \varepsilon_m (\gamma_m^\dagger \gamma_m - \frac12)$:
```wl
snegspinlessfermionoperators[g];
snegrealconstants[e1, e2];
quadraticoperator[DiagonalMatrix[{e1, e2, -e1, -e2}], {g[1], g[2]}]
```

### Possible Issues
The dimension of the matrix must be $n$ or $2n$ for $n$ modes. For spin-1/2 fermions, each level contributes two modes:
```wl
snegfermionoperators[c];
quadraticoperator[{{1, 0}, {0, 1}}, {c[1], c[2]}]
```
---
The matrix is used as given and not checked. A non-Hermitian hopping matrix gives a non-Hermitian operator:
```wl
snegspinlessfermionoperators[f];
quadraticoperator[{{0, 1}, {0, 0}}, {f[1], f[2]}]
```
