---
keywords: spin component index, Cartesian index, summation index, Einstein summation, Levi-Civita
see also: levicivita, spincomponents, spincomponentindexQ, listspincomponentindexes, snegspinoperators, snegfreeindexes, sum
---
# snegspincomponentindexes

## Usage
- `snegspincomponentindexes[a, b, ...]` declares *a*, *b*, ... as symbolic Cartesian spin component indexes with the values 1, 2, 3 ($x$, $y$, $z$).

## Details
- Spin component indexes are summation indexes (they are also declared with `snegfreeindexes`) and real: `Conjugate[a]` is *a*, so that `conj[S[j, a]]` is `S[j, a]`.
- The commutator of spin operators with symbolic components is $[S^a_i, S^b_j] = i \delta_{ij} \sum_c \varepsilon_{abc} S^c_j$, returned as `I sum[levicivita[a, b, c] S[j, c], {c}]` with a new dummy index *c*. Components 4 and 5 ($S^\pm$) are combined with symbolic components through $S^\pm = S^x \pm i S^y$.
- A sum over a component index of an expression that does not contain it gives a factor 3.
- `spincomponents` expands sums over component indexes into explicit components.
- The declared indexes are listed in `listspincomponentindexes` and recognized by `spincomponentindexQ`.

## Examples
### Basic Examples
```wl
snegspinoperators[S];
snegspincomponentindexes[a, b];
komutator[S[1, a], S[1, b]]
```
Operators on different sites commute:
```wl
komutator[S[1, a], S[2, b]]
```

### Scope
The scalar product written as a sum over a component index:
```wl
snegspinoperators[S];
snegspincomponentindexes[a];
spincomponents[sum[nc[S[1, a], S[2, a]], {a}]] == spincomponents[spinspin[S[1], S[2]]]
```
---
Explicit components can be substituted:
```wl
snegspinoperators[S];
snegspincomponentindexes[a, b];
spincomponents[komutator[S[1, a], S[1, b]] /. {a -> 1, b -> 2}]
```
---
A sum over a component index that does not appear in the summand:
```wl
snegspinoperators[S];
snegspincomponentindexes[a];
sum[S[1, 3], {a}]
```

### Properties & Relations
```wl
snegspincomponentindexes[a];
{spincomponentindexQ[a], conj[S[1, a]], MemberQ[listspincomponentindexes, a]}
```
