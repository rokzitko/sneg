---
keywords: commutator, Heisenberg equation of motion, conserved quantity, Lie bracket, Jacobi identity
see also: antikomutator, superkomutator, nc, liouvillian, eomclosure, SnegSimplify
---
# komutator

## Usage
- `komutator[a, b]` gives the commutator $[a, b] = a b - b a$.

## Details
- `komutator[a, b]` is `nc[a, b] - nc[b, a]`, with both products brought to the canonical order. `commutator` is a synonym.
- The result is not expanded. Use `Expand` to collect the terms; a commutator that vanishes is only reduced to 0 after `Expand`.
- Spin operators obey the su(2) algebra $[S^a_i, S^b_j] = i \delta_{ij} \epsilon_{abc} S^c_i$. For symbolic Cartesian components declared with `snegspincomponentindexes`, the commutator is given directly in terms of `levicivita`. Operators of different spin symbols commute.
- Commutators of sums `sum[...]` are computed with automatic renaming of dummy indexes. Use `sumExpand` and `sumSimplifyKD` to perform the sums over Kronecker deltas.
- `komutator` is `Listable`: `komutator[{a1, a2}, b]` gives `{komutator[a1, b], komutator[a2, b]}`.
- The time evolution of an operator in the Heisenberg picture is $i\hbar\, \dot A = [A, H]$; `liouvillian` gives iterated commutators $[[A, H], H], \ldots$.

## Developer Notes
- `komutator` has rules for spin operators (`spinQ`) and for sums; all other cases use `nc`. To change the commutation relations of an operator family, redefine `cmt` or `acmt`, which are used by `nc` to reorder operators; `komutator` follows automatically.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
komutator[c[CR, 1, UP], c[AN, 2, UP]]
```
Commutator of the number operator with a creation operator, $[n, c^\dagger] = c^\dagger$:
```wl
Expand[komutator[nc[c[CR, 1, UP], c[AN, 1, UP]], c[CR, 1, UP]]]
```

### Scope
Spin operators obey $[S^+, S^-] = 2 S^z$ and $[S^x, S^y] = i S^z$ on the same site, and commute on different sites:
```wl
snegspinoperators[{S, 1/2}];
{komutator[S[1, 4], S[1, 5]], komutator[S[1, 1], S[1, 2]], komutator[S[1, 1], S[2, 2]]}
```
---
The spin of a fermion level, $S^a = \frac12 \sum_{\sigma\sigma'} c^\dagger_\sigma \sigma^a_{\sigma\sigma'} c_{\sigma'}$, obeys the same algebra:
```wl
snegfermionoperators[d];
{sx, sy, sz} = spinxyz[d[]];
Expand[komutator[sx, sy] - I sz]
```
---
Commutators with sums. For a band Hamiltonian $H = \sum_k \epsilon_k n_{k\uparrow}$, $[c_{p\uparrow}, H] = \epsilon_p c_{p\uparrow}$:
```wl
snegfermionoperators[c];
snegfreeindexes[k, p];
snegrealfunctions[eps];
H = sum[eps[k] number[c[k], UP], {k}];
sumSimplifyKD[sumExpand[komutator[c[AN, p, UP], H]]]
```
---
`komutator` threads over lists:
```wl
komutator[{c[CR, 1, UP], c[CR, 2, UP]}, number[c[1]]]
```

### Applications
The total particle number $N = n_1 + n_2$ is conserved in the Hubbard dimer, $[N, H] = 0$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
Expand[komutator[number[c[1]] + number[c[2]], H]]
```
The occupancy of one site is not conserved; its time derivative is the current between the sites:
```wl
Expand[komutator[number[c[1]], H]]
```
---
Equation of motion of the annihilation operator, $[c_{1\uparrow}, H]$:
```wl
Expand[komutator[c[AN, 1, UP], H]]
```

### Properties & Relations
The Jacobi identity holds for any operators:
```wl
Expand[komutator[x, komutator[y, z]] + komutator[y, komutator[z, x]] + komutator[z, komutator[x, y]]]
```
---
Relation with the anticommutator, $\{a, b\} - [a, b] = 2 b a$:
```wl
snegfermionoperators[c];
Expand[antikomutator[c[AN, 1, UP], c[CR, 1, UP]] - komutator[c[AN, 1, UP], c[CR, 1, UP]] - 2 nc[c[CR, 1, UP], c[AN, 1, UP]]]
```
---
The commutator of two fermion operators is not a c-number; the canonical relations are anticommutators:
```wl
{komutator[c[AN, 1, UP], c[CR, 1, UP]], antikomutator[c[AN, 1, UP], c[CR, 1, UP]]}
```

### Possible Issues
The result is not expanded, so a vanishing commutator may not look like zero. The total spin projection is conserved by hopping:
```wl
snegfermionoperators[c];
komutator[spinz[c[1]] + spinz[c[2]], hop[c[1], c[2]]]
```
```wl
Expand[%]
```
