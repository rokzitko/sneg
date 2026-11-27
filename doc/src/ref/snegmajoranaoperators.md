---
keywords: Majorana fermion, real fermion, Clifford algebra, declaration, Kitaev chain, topological superconductor
see also: snegfermionoperators, snegspinlessfermionoperators, majoranaQ, acmt, conj, listmajoranaoperators
---
# snegmajoranaoperators

## Usage
- `snegmajoranaoperators[g, h, ...]` declares *g*, *h*, ... as Majorana (real) fermion operators.

## Details
- A Majorana operator is written as `g[j, ...]` with arbitrary indexes, or as the bare symbol `g` for a single mode. It is Hermitian, $\gamma^\dagger = \gamma$, and there is no creation/annihilation index.
- The operators obey $\{\gamma_i, \gamma_j\} = \delta_{ij}$, i.e. $\gamma_i^2 = 1/2$. This is the normalization of $\gamma = (c + c^\dagger)/\sqrt2$; with the other common convention $\{\gamma_i, \gamma_j\} = 2\delta_{ij}$, the operators differ by the factor $\sqrt 2$.
- Products are ordered with `OrderedQ` (by indexes, then alphabetically for different families). Majorana operators of different families anticommute. Majorana operators anticommute with Dirac fermions and Grassmann numbers and are moved to the left of Dirac fermions; they commute with bosons.
- The declaration sets `operatorQ[g]` and `majoranaQ[g]` to `True`, defines `conj[g] = g`, adds `g` to `listmajoranaoperators`, and gives `g` the attribute `NHoldAll`. The anticommutator is given by `acmt`.
- There is no pretty-printing for Majorana operators.

## Examples
### Basic Examples
```wl
snegmajoranaoperators[g];
{nc[g[1], g[1]], nc[g[2], g[1]], antikomutator[g[1], g[2]], conj[g[1]]}
```

### Scope
Single-mode Majorana operators written as bare symbols, and two families:
```wl
snegmajoranaoperators[g, h];
{nc[g, g], nc[h, g], nc[h[1], g[1]]}
```
---
Majorana and Dirac fermions anticommute:
```wl
snegmajoranaoperators[g];
snegfermionoperators[c];
nc[c[CR, 1, UP], g[1]]
```

### Applications
A Dirac fermion as a pair of Majorana operators, $c = (\gamma_1 + i\gamma_2)/\sqrt2$, obeys the canonical anticommutation relations, and the occupation is $n = 1/2 + i\gamma_1\gamma_2$:
```wl
snegmajoranaoperators[g];
cc = (g[1] + I g[2])/Sqrt[2];
cd = conj[cc];
{Expand[antikomutator[cc, cd]], Expand[nc[cc, cc]], Expand[nc[cd, cc]]}
```
---
The parity operator $P = -2i\gamma_1\gamma_2$ of a pair squares to one, and it anticommutes with each Majorana operator of the pair:
```wl
snegmajoranaoperators[g];
par = -2 I nc[g[1], g[2]];
{Expand[pow[par, 2]], Expand[antikomutator[par, g[1]]]}
```

### Properties & Relations
The type predicates. Majorana operators are not Dirac fermions; `fermionQ` is not defined for them:
```wl
snegmajoranaoperators[g];
{operatorQ[g], majoranaQ[g], majoranaQ[g[1]], fermionQ[g]}
```

### Possible Issues
With the normalization $\{\gamma, \gamma\} = 1$, the square of a Majorana operator is 1/2, not 1:
```wl
snegmajoranaoperators[g];
pow[g[1], 2]
```
