---
keywords: Hubbard X-operator, Hubbard operator, projection operator, atomic limit, t-J model, SU(N), strong correlations, declaration
see also: xoperator, xtofermions, fermionstox, xtospin, xoperatorQ, listxoperators, snegspinoperators, schriefferwolff
---
# snegxoperators

## Usage
- `snegxoperators[X, {a1, a2, ...}, {p1, p2, ...}]` declares *X* as Hubbard X-operators for the local states *a1*, *a2*, ... with the fermion parities *p1*, *p2*, ... (0 or 1).
- `snegxoperators[X, "HubbardAtom"]` declares *X* for the four states `"0"`, `"UP"`, `"DO"`, `"2"` of a spin-1/2 orbital.

## Details
- `X[j, ..., a, b]` is the operator $X^{ab}_j = |a\rangle_j\langle b|$ on the site `j, ...` (there may be no site index); the last two indexes are the states. In output it is shown as $X^{ab}_j$.
- The states carry fermion parities: $X^{ab}$ is a fermionic operator if the parities of *a* and *b* differ, and bosonic otherwise.
- On the same site, $X^{ab} X^{cd} = \delta_{bc} X^{ad}$. Operators on different sites commute, or anticommute if both are fermionic. For symbolic site indexes, the reordering gives $X_i^{ab} X_j^{cd} = \pm X_j^{cd} X_i^{ab} + \delta_{ij}(\delta_{bc} X^{ad} \mp \delta_{da} X^{cb})$. Products are ordered by site.
- $(X^{ab})^\dagger = X^{ba}$.
- The completeness relation $\sum_a X^{aa} = 1$ is not applied automatically.
- For `"HubbardAtom"`, the states are $|0\rangle$, $|{\uparrow}\rangle = c^\dagger_\uparrow|0\rangle$, $|{\downarrow}\rangle = c^\dagger_\downarrow|0\rangle$, $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$, with the parities 0, 1, 1, 0. `xoperator` gives the X-operators in terms of fermions, and `xtofermions` and `fermionstox` convert expressions; `xtospin` maps the operators of singly occupied sites to spin operators.
- X-operators are independent of all other operators: they are moved to the right of spin operators and to the left of bosons, Majorana and Dirac fermions, anticommuting with fermions when fermionic. They should therefore not be mixed with fermion operators of the same site.
- The declaration sets `operatorQ[X]` and `xoperatorQ[X]` to `True`, adds *X* to `listxoperators`, and gives *X* the attribute `NHoldAll`. Invalid arguments (lists of different lengths, repeated states, parities other than 0 and 1) give the message `snegxoperators::spec` and `$Failed`.
- With bosonic states only, `snegxoperators[Y, {1, ..., N}, {0, ..., 0}]` gives the generators of SU(N) in the fundamental representation (SU(N) spins).

## Examples
### Basic Examples
```wl
snegxoperators[X, "HubbardAtom"];
{X[1, "0", "UP"], nc[X[1, "0", "UP"], X[1, "UP", "2"]], nc[X[1, "0", "UP"], X[1, "DO", "2"]]}
```
The spin operators of a singly occupied site, $S^+ = X^{\uparrow\downarrow}$, $S^z = (X^{\uparrow\uparrow} - X^{\downarrow\downarrow})/2$, obey $[S^+, S^-] = 2S^z$:
```wl
komutator[X[1, "UP", "DO"], X[1, "DO", "UP"]]
```

### Scope
Fermionic X-operators on different sites anticommute; bosonic ones commute:
```wl
snegxoperators[X, "HubbardAtom"];
{nc[X[2, "UP", "0"], X[1, "0", "UP"]], nc[X[2, "UP", "DO"], X[1, "0", "UP"]]}
```
Symbolic site indexes; $\{X^{0\uparrow}_i, X^{\uparrow 0}_j\} = \delta_{ij}(X^{00} + X^{\uparrow\uparrow})$:
```wl
antikomutator[X[i, "0", "UP"], X[j, "UP", "0"]]
```
---
Three bosonic states give SU(3) spins:
```wl
snegxoperators[Y, {1, 2, 3}, {0, 0, 0}];
{komutator[Y[1, 1, 2], Y[1, 2, 3]], komutator[Y[1, 1, 2], Y[1, 2, 1]]}
```
---
X-operators of the Hubbard atom in terms of fermion operators, e.g. $X^{0\uparrow} = c_\uparrow(1 - n_\downarrow)$:
```wl
snegxoperators[X, "HubbardAtom"];
snegfermionoperators[c];
xoperator[c[1], "0", "UP"]
```

### Applications
The t-J model on a dimer, $H = -t\sum_\sigma (X_1^{\sigma 0} X_2^{0\sigma} + \mathrm{h.c.}) + J(\mathbf S_1\cdot\mathbf S_2 - n_1 n_2/4)$, the large-$U$ limit of the Hubbard dimer with $J = 4t^2/U$. The spin operators and the occupation are written with X-operators:
```wl
snegxoperators[X, "HubbardAtom"];
snegfermionoperators[c];
snegrealconstants[t, J];
sz[i_] := (X[i, "UP", "UP"] - X[i, "DO", "DO"])/2;
nn[i_] := X[i, "UP", "UP"] + X[i, "DO", "DO"];
HtJ = -t Sum[nc[X[1, s, "0"], X[2, "0", s]] + nc[X[2, s, "0"], X[1, "0", s]], {s, {"UP", "DO"}}] + J (nc[sz[1], sz[2]] + (nc[X[1, "UP", "DO"], X[2, "DO", "UP"]] + nc[X[1, "DO", "UP"], X[2, "UP", "DO"]])/2 - nc[nn[1], nn[2]]/4);
```
With two electrons, the singlet has the energy $-J$ and the triplet 0. The energies are computed with fermion operators after `xtofermions`:
```wl
Hf = xtofermions[HtJ, X -> c];
singlet = (nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];
triplet = nc[c[CR, 1, UP], c[CR, 2, UP]];
Simplify[{vev[nc[conj[singlet], Hf, singlet]], vev[nc[conj[triplet], Hf, triplet]]}]
```

### Properties & Relations
The declaration defines the type predicates:
```wl
snegxoperators[X, "HubbardAtom"];
{operatorQ[X], xoperatorQ[X], xoperatorQ[X[1, "0", "UP"]], MemberQ[listxoperators, X]}
```
---
Conjugation exchanges the states:
```wl
snegxoperators[X, "HubbardAtom"];
conj[nc[X[1, "UP", "0"], X[2, "DO", "UP"]]]
```

### Possible Issues
The completeness relation is not used; here $\sum_{ab} X^{ab} X^{ba} = 3\sum_a X^{aa} = 3$ for three states:
```wl
snegxoperators[Y, {1, 2, 3}, {0, 0, 0}];
Sum[nc[Y[1, a, b], Y[1, b, a]], {a, 3}, {b, 3}]
```
---
The states and parities must be lists of equal length:
```wl
snegxoperators[Z, {1, 2}, {0}]
```
