---
keywords: bra-ket representation, spin operator, ket, bra, spin-s, impurity spin, NRG
see also: spinketbraY, spinketbraZ, spinketbraP, spinketbraM, spinketbraI, spinketbraXYZ, spinketbraOPS, spinketbrarules, spinket, spinbasis, spinmatrixX, ket, bra
---
# spinketbraX

## Usage
- `spinketbraX[s]` gives the $x$ component of the spin-*s* operator in the bra-ket representation, $S^x = \sum_{mm'} |m\rangle (S^x)_{mm'} \langle m'|$.
- `spinketbraX[s, pos]` places the spin in the slots of the kets and bras specified by the position array *pos*.

## Details
- The result is a linear combination of products `nc[ket[m], bra[m']]` with the matrix elements of `spinmatrixX[s]`, $m, m' = s, \ldots, -s$. The analogous functions are `spinketbraY`, `spinketbraZ`, `spinketbraP` ($S^+$), `spinketbraM` ($S^-$), and `spinketbraI` (identity).
- In this representation, spin operators act on the spin kets `ket[m]` by the rule $\langle m|m'\rangle = \delta_{mm'}$ (`braketrule`). The spin kets can be combined with fermion states in the occupation-number representation, `vc[..., ket[m]]`, which is used by NRG Ljubljana for impurity spins coupled to fermions.
- The position array *pos* is a list of 0 and 1 of the length of the full kets; the spin is placed in the slot(s) with 1, and the other slots are `Null`. For two spins, `spinketbraX[s1, {1, 0}]` acts on the first slot of `ket[m1, m2]` and `spinketbraX[s2, {0, 1}]` on the second; the product `nc` of operators on different slots combines them into operators on the full kets. The default is `{1}`.
- An operator on a single slot does not act on full kets by itself; multiply it with the identities `spinketbraI` of the other slots. `spinketbrarules` does this automatically for abstract spin operators.
- *s* must be an integer or a half-integer.

## Examples
### Basic Examples
```wl
spinketbraX[1/2]
```
Acting on the spin kets:
```wl
{nc[spinketbraX[1/2], ket[1/2]], nc[spinketbraX[1/2], ket[-1/2]]}
```

### Scope
Spin 1:
```wl
spinketbraX[1]
```
---
The spin in the second of two slots:
```wl
spinketbraX[1/2, {0, 1}]
```

### Applications
Exchange of two spins 1/2, built from operators on the two slots. The matrices in the basis `spinbasis[{1/2, 1/2}]` give the triplet $1/4$ and the singlet $-3/4$:
```wl
s1 = spinketbraXYZ[1/2, {1, 0}];
s2 = spinketbraXYZ[1/2, {0, 1}];
h = Expand[Total[MapThread[nc, {s1, s2}]]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzop[h, spinbasis[{1/2, 1/2}]]
```

### Properties & Relations
The matrix elements are those of `spinmatrixX`:
```wl
matrixrepresentationop[spinketbraX[3/2], spinket[3/2]] == spinmatrixX[3/2]
```
---
The su(2) algebra holds in the bra-ket representation:
```wl
Expand[nc[spinketbraX[1], spinketbraY[1]] - nc[spinketbraY[1], spinketbraX[1]] - I spinketbraZ[1]]
```

### Possible Issues
An operator on one slot does not act on a full two-slot ket:
```wl
nc[spinketbraZ[1/2, {1, 0}], ket[1/2, -1/2]]
```
Include the identity of the other slot:
```wl
nc[spinketbraZ[1/2, {1, 0}], spinketbraI[1/2, {0, 1}], ket[1/2, -1/2]]
```
