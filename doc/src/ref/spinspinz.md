---
keywords: longitudinal exchange, Ising interaction, XXZ model, anisotropic exchange
see also: spinspin, spinspinxy, spinspinpm, spinspinmp, spinz
---
# spinspinz

## Usage
- `spinspinz[a[i], b[j]]` gives the longitudinal part $S^z_{a,i} S^z_{b,j}$ of the exchange interaction between the fermion levels `a[i]` and `b[j]`.

## Details
- `spinspinz[a, b]` is `nc[spinz[a], spinz[b]]`; see `spinxyz` for the spin operators.
- `spinspin[a, b] == spinspinz[a, b] + spinspinxy[a, b]`. The anisotropic (XXZ) exchange is $J_z S^z_a S^z_b + J_\perp (S^x_a S^x_b + S^y_a S^y_b)$.
- The levels must be given with brackets, `c[]` or `c[i]`; abstract spin operators are not supported.
- The result is not expanded.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinspinz[c[1], c[2]]
```

### Applications
Anisotropic exchange between two singly occupied levels, $H = J_z S^z_1 S^z_2 + J_\perp (S^x_1 S^x_2 + S^y_1 S^y_2)$. In the half-filled subspace ($Q = 0$, $S_z = 0$) the eigenvalues include $-J_z/4 \pm J_\perp/2$ (singlet and $S_z = 0$ triplet):
```wl
snegfermionoperators[c];
snegrealconstants[Jz, Jp];
H = Jz spinspinz[c[1], c[2]] + Jp spinspinxy[c[1], c[2]];
Eigenvalues[Last[First[Select[makematricesbzvc[H, qszbasisvc[{c[1], c[2]}]], First[#] == {0, 0} &]]]]
```

### Properties & Relations
```wl
snegfermionoperators[c];
spinspinz[c[1], c[2]] == nc[spinz[c[1]], spinz[c[2]]]
```
