---
keywords: simplification, rewrite, number operator, Hubbard interaction, hopping, spin-spin interaction, readable output
see also: number, hubbard, hop, twohop, spinspin, chargecharge, spinxyz, ReleaseHold, Simplify
---
# SnegSimplify

## Usage
- `SnegSimplify[expr]` rewrites the operator expression *expr* in terms of the SNEG functions `number`, `hubbard`, `hop`, `twohop`, `spinx`, `spiny`, `spinz`, `spinspin`, and `chargecharge`, wherever their expanded forms are recognized.

## Details
- `SnegSimplify` makes expanded operator expressions readable. The recognized terms are returned wrapped in `HoldForm`, e.g. `HoldForm[hubbard[c[1]]]`, so that they are not expanded again; `ReleaseHold` gives back the operator expression.
- The expression is first collected with respect to the declared constants; the number operators, Hubbard terms, hopping terms, spin operators and spin-spin products, and density-density terms are then replaced in this order.
- The rules recognize the terms of fermion operators with spin 1/2 in the form produced by `nc` (normal-ordered, expanded). Terms that do not match are left unchanged.
- `SnegFullSimplify[expr]` uses the same replacements as transformation functions of `Simplify` and searches for the simplest combination. It finds more compact forms, e.g. $\mathbf S_1 \cdot \mathbf S_2 - n_1 n_2/4$, but is slower.
- Number operators of the same level are replaced first, so that $S^z$ is given as $(n_\uparrow - n_\downarrow)/2$.

## Developer Notes
- `SnegSimplify` applies the rule sets `SnegSimplifyNumber`, `SnegSimplifyHubbard`, `SnegSimplifyHop`, `SnegSimplifySpin`, and `SnegSimplifyMisc` (private, except `SnegSimplifySpin`). The rules are generated from the definitions of `spinxyz`, `hop`, and the other functions with placeholder operators, so they follow changes of these definitions.

## Examples
### Basic Examples
The Hubbard dimer in expanded form:
```wl
snegfermionoperators[c];
snegrealconstants[t, U, ε];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]) + ε (number[c[1]] + number[c[2]])
```
```wl
SnegSimplify[H]
```

### Scope
Spin-spin interactions:
```wl
snegfermionoperators[c];
snegrealconstants[J, U];
SnegSimplify[Expand[J spinspin[c[1], c[2]] + U hubbard[c[1]]]]
```
---
Hopping terms on several bonds:
```wl
snegfermionoperators[c];
snegrealconstants[t];
SnegSimplify[Expand[2 t hop[c[1], c[2]] + t hop[c[2], c[3]]]]
```
---
Density-density interaction:
```wl
snegfermionoperators[c];
SnegSimplify[Expand[nc[number[c[1]], number[c[2]]]]]
```

### Properties & Relations
`ReleaseHold` gives back the operator expression:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U hubbard[c[1]];
Expand[ReleaseHold[SnegSimplify[H]] - H]
```
---
`SnegFullSimplify` finds the combination $J(\mathbf S_1 \cdot \mathbf S_2 - n_1 n_2/4)$, the form of the superexchange interaction of the Hubbard model, which `SnegSimplify` writes in terms of the spin components:
```wl
snegfermionoperators[c];
snegrealconstants[J];
x = Expand[J spinspin[c[1], c[2]] - J/4 nc[number[c[1]], number[c[2]]]];
{SnegSimplify[x], SnegFullSimplify[x]}
```

### Possible Issues
The spin projection is written in terms of number operators; in the held forms, the spin indexes appear as 1 (`UP`) and 0 (`DO`):
```wl
snegfermionoperators[c];
SnegSimplify[spinz[c[1]]]
```
---
Combinations with complex coefficients may not be recognized:
```wl
snegfermionoperators[c];
SnegSimplify[Expand[spinx[c[1]] + spiny[c[1]]]]
```
