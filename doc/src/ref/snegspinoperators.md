---
keywords: spin operator, su(2) algebra, local moment, Heisenberg model, Kondo model, spin size, Pauli algebra, Casimir operator, declaration
see also: spincomponents, snegspincomponentindexes, spinclustermatrices, spinspin, spinQ, spinof, cmt, jordanwigner, holsteinprimakoff, schwingerbosons, abrikosov, stevens, spintensor, rotatespin, levicivita, listspinoperators
---
# snegspinoperators

## Usage
- `snegspinoperators[S, ...]` declares *S* as spin operators that obey the su(2) algebra, without a specified spin size.
- `snegspinoperators[{S, s}, ...]` declares *S* as spin operators of size *s*, a positive integer or half-integer or a symbolic expression.

## Details
- A spin operator is written as `S[j, ..., t]`: the indexes `j, ...` label the site (there may be none), and the last index is the component `t` = 1, 2, 3, 4, 5 for $S^x$, $S^y$, $S^z$, $S^+$, $S^-$. In output, `S[j, 3]` is shown as $S^z_j$.
- Operators on the same site obey $[S^a, S^b] = i\epsilon_{abc} S^c$, $[S^z, S^\pm] = \pm S^\pm$, $[S^+, S^-] = 2S^z$; operators on different sites commute. For symbolic site indexes, commutators contain $\delta_{jj'}$.
- Without a declared size, only these commutation relations are used, which hold in every representation. Products are sorted by site and component ($x < y < z < + < -$), and commutators are applied when operators are exchanged.
- With a declared size *s*, products of operators on the same site are reduced to the normal form $\sum_{a,b} c_{ab} (S^z)^b (S^\pm)^a$:
  - for every size, $S^+ S^- = s(s+1) - (S^z)^2 + S^z$, so that $\mathbf S\cdot\mathbf S = s(s+1)$, and $S^x$, $S^y$ in same-site products are written with $S^\pm$;
  - for numeric *s*, also the products with $a + b > 2s$ are reduced, e.g. $(S^\pm)^{2s+1} = 0$. The remaining $(2s+1)^2$ monomials are linearly independent, so the normal form is unique and operator identities reduce to zero;
  - for $s = 1/2$, products of Cartesian components follow the Pauli rule $S^a S^b = \delta_{ab}/4 + (i/2)\epsilon_{abc}S^c$ and stay Cartesian.
- A symbolic size, e.g. `snegspinoperators[{S, s}]`, declares the symbols in it as real constants.
- Spin operators commute with all other operators. In products they are moved to the left of bosons, Majorana fermions, Dirac fermions, and X-operators; different spin symbols are sorted alphabetically.
- `conj` gives $(S^{x,y,z})^\dagger = S^{x,y,z}$ and $(S^\pm)^\dagger = S^\mp$.
- The declaration sets `operatorQ[S]` and `spinQ[S]` to `True`, `spinof[S]` to *s*, adds *S* to `listspinoperators`, and gives *S* the attribute `NHoldAll`. A size that is not a positive half-integer or a symbolic expression gives the message `snegspinoperators::size`.
- Spin operators of fermion levels, $\mathbf S = \frac12 \sum_{\sigma\sigma'} c^\dagger_\sigma \boldsymbol\sigma_{\sigma\sigma'} c_{\sigma'}$, are given by `spinxyz` and related functions; they are operator expressions, not declared spin operators.

## Developer Notes
- The commutators are defined by `cmt` and the order by `snegOrderedQ` (`OrderedQ` of the operators). The same-site reduction for a declared size is computed once for each sequence of components from the spin matrices (`spinmatrixX`, ...) and cached.
- Because the last index is the component, `S[j, 0]` evaluates to 0 and `S[j, -t]` to `-S[j, t]` for `t` = 1, 2, 3. Functions that take lists of sites accept `S -> {0, 1, ...}` instead of `{S[0], S[1], ...}`.

## Examples
### Basic Examples
Declare spin-1/2 operators:
```wl
snegspinoperators[{S, 1/2}];
{S[1, 1], S[1, 3], S[1, 4], S[1, 5]}
```
Commutation relations and same-site products:
```wl
{komutator[S[1, 1], S[1, 2]], nc[S[1, 1], S[1, 2]], nc[S[1, 4], S[1, 5]], spinspin[S[1], S[1]]}
```
Operators on different sites commute:
```wl
komutator[S[1, 4], S[2, 5]]
```

### Scope
Spin 1: $(S^+)^3 = 0$, $(S^z)^3 = S^z$, and $\mathbf S^2 = 2$:
```wl
snegspinoperators[{T, 1}];
{nc[T[1, 4], T[1, 4], T[1, 4]], nc[T[1, 3], T[1, 3], T[1, 3]], spinspin[T[1], T[1]]}
```
---
A symbolic spin size; $S^- S^+ = s(s+1) - (S^z)^2 - S^z$:
```wl
snegspinoperators[{M, s}];
{nc[M[1, 5], M[1, 4]], spinof[M]}
```
---
Without a size, only the Lie algebra is used; same-site products are only ordered:
```wl
snegspinoperators[G];
{nc[G[1, 5], G[1, 4]], komutator[G[1, 1], G[1, 2]], spinspin[G[1], G[1]]}
```
`spincomponents` rewrites in $S^z, S^\pm$; with the automatic ordering, this gives a unique form, so that identities reduce to zero, e.g. $[\mathbf S_1\cdot\mathbf S_2, S^+_1 + S^+_2] = 0$:
```wl
spincomponents[Expand[komutator[spinspin[G[1], G[2]], G[1, 4] + G[2, 4]]]]
```
---
Symbolic site indexes:
```wl
snegspinoperators[{S, 1/2}];
komutator[S[i, 4], S[j, 5]]
```
---
Symbolic Cartesian components declared with `snegspincomponentindexes`:
```wl
snegspinoperators[S];
snegspincomponentindexes[a, b];
komutator[S[1, a], S[1, b]]
```
---
Spin operators commute with other operators and are moved to the left:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
nc[c[CR, 1, UP], S[3], c[AN, 1, UP]]
```

### Applications
Heisenberg ring of four spins 1/2, $H = J\sum_i \mathbf S_i\cdot\mathbf S_{i+1}$: the lowest energy in each subspace of total $S^z$; the singlet ground state has $E = -2J$:
```wl
snegspinoperators[{S, 1/2}];
snegrealconstants[J];
H = J Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}];
{First[#], Min[Eigenvalues[Last[#]]]} & /@ spinclustermatrices[H /. J -> 1, {S -> Range[4]}]
```
---
Kondo coupling of an impurity spin with the spin of a conduction-electron level, $H_K = J\,\mathbf S\cdot\mathbf s$. The total $S^z$ is conserved:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
snegrealconstants[J];
HK = J (nc[S[3], spinz[c[]]] + (nc[S[4], spinminus[c[]]] + nc[S[5], spinplus[c[]]])/2);
Expand[komutator[HK, S[3] + spinz[c[]]]]
```
---
Single-ion anisotropy of a spin 1, $H = D (S^z)^2$: the doublet $m = \pm 1$ at $D$ (`Dz`) and the singlet $m = 0$ at 0. The square of $(S^z)^2$ is reduced to $(S^z)^2$:
```wl
snegspinoperators[{T, 1}];
snegrealconstants[Dz];
Hani = Dz nc[T[1, 3], T[1, 3]];
{pow[Hani, 2], spinclustermatrices[Hani, {T[1]}, "Blocks" -> False]}
```

### Properties & Relations
The declaration defines the type predicates and the size:
```wl
snegspinoperators[{S, 1/2}];
{operatorQ[S], spinQ[S], spinQ[S[1, 3]], spinof[S], MemberQ[listspinoperators, S]}
```
---
Conjugation:
```wl
snegspinoperators[{S, 1/2}];
{conj[S[1, 4]], conj[S[1, 2]], conj[nc[S[1, 4], S[2, 3]]]}
```
---
The spin operators of a fermion level obey the same algebra, but they are operator expressions:
```wl
snegfermionoperators[c];
{sx, sy, sz} = spinxyz[c[]];
Expand[komutator[sx, sy] - I sz]
```

### Possible Issues
The last index is the component, not the site: `S[1]` is $S^x$ of a spin without site index, and `S[0]` is zero:
```wl
snegspinoperators[{S, 1/2}];
{S[1], S[0], S[-3]}
```
---
For a spin without declared size, `spinof` gives the default value 1/2, although no size is used in the calculations:
```wl
snegspinoperators[G];
{spinof[G], nc[G[1, 4], G[1, 5]]}
```
---
An invalid spin size gives a message, and the symbol is not declared:
```wl
snegspinoperators[{Q, -1}];
spinQ[Q]
```
