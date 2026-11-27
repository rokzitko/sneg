---
keywords: number operator, occupation number, particle number, charge, density, n_i, b^dagger b, U(1) symmetry, charge conservation
see also: hop, hubbard, chargecharge, isozsq, projector, spinz, isospinz, snegfermionoperators, snegbosonoperators, qszbasisvc
---
# number

## Usage
- `number[c[i]]` gives the number operator $n_i = \sum_\sigma c^\dagger_{i\sigma} c_{i\sigma}$ of the fermion level `c[i]`.
- `number[c[i], σ]` gives the number operator $n_{i\sigma} = c^\dagger_{i\sigma} c_{i\sigma}$ for the spin projection *σ* only.
- `number[b[i]]` gives the number operator $b^\dagger_i b_i$ of the boson mode `b[i]`.
- `number[c]` is `number[c[]]`.

## Details
- The level is specified without the creation/annihilation and spin indexes: `c[i]` stands for the operators `c[CR, i, σ]` and `c[AN, i, σ]`. The level may have any number of indexes, `c[]`, `c[i]`, `c[i, α]`, ....
- `number[c[i], σ]` is `nc[c[CR, i, σ], c[AN, i, σ]]` for any *σ*, also symbolic.
- For spin-1/2 fermions, $\sigma \in \{\uparrow, \downarrow\}$ (`UP`, `DO`). For fermions with spin $s$, declared with `snegfermionoperators[{c, s}]`, the sum runs over $\sigma = -s, \ldots, s$. Spinless fermions (`snegspinlessfermionoperators`) carry the spin index 0, as in the basis constructors: `number[f[i]]` is `nc[f[CR, i, 0], f[AN, i, 0]]`.
- For bosons, `number[b[i]]` is `nc[b[CR, i], b[AN, i]]`; there is no spin index.
- The level can also be an abstract orbital, a pure function `fn` with `fn[CR, σ]` the creation operator of the orbital (a linear combination of creation operators). Then `number[fn, σ]` is $a^\dagger_\sigma a_\sigma$ with $a^\dagger_\sigma$ = `fn[CR, σ]` and $a_\sigma$ = `conj[fn[CR, σ]]`, and `number[fn]` sums over `UP` and `DO`. The orbital should be normalized for this to be a number operator.
- The result is in normal order. The eigenvalues of $n_{i\sigma}$ are 0 and 1, those of $n_i$ for a spin-1/2 level are 0, 1, 2.
- The total number operator $N = \sum_i n_i$ generates the U(1) charge symmetry: $[N, c^\dagger_{i\sigma}] = c^\dagger_{i\sigma}$. The charge quantum number $Q = N - N_\mathrm{half}$ of `qszbasisvc` is measured from half filling.
- `number` is `Listable`.

## Examples
### Basic Examples
The number operator of a spin-1/2 level, $n = n_\uparrow + n_\downarrow$:
```wl
snegfermionoperators[c];
number[c[]]
```
The spin-up part $n_\uparrow = c^\dagger_\uparrow c_\uparrow$:
```wl
number[c[], UP]
```

### Scope
Levels with indexes, for example site and orbital:
```wl
snegfermionoperators[c];
{number[c[1]], number[c[1, 2], DO]}
```
---
`number` threads over lists of levels. The total number operator of three sites:
```wl
Ntot = Total[number[{c[1], c[2], c[3]}]]
```
---
A symbol is a level without indexes:
```wl
number[c] === number[c[]]
```
---
Symbolic spin index:
```wl
snegfreeindexes[σ];
number[c[1], σ]
```
---
Boson number operator $b^\dagger b$:
```wl
snegbosonoperators[b];
number[b[]]
```
---
For fermions with spin 1, the sum runs over $\sigma = -1, 0, 1$:
```wl
snegfermionoperators[{f, 1}];
number[f[]]
```
---
Spinless fermions carry the spin index 0:
```wl
snegspinlessfermionoperators[a];
number[a[1]]
```
---
Number operator of the bonding orbital $a^\dagger_\sigma = (c^\dagger_{1\sigma} + c^\dagger_{2\sigma})/\sqrt{2}$, given as a pure function of the type and spin indexes:
```wl
snegfermionoperators[c];
bond = (c[#1, 1, #2] + c[#1, 2, #2])/Sqrt[2] &;
number[bond, UP]
```

### Applications
Particle number of a state: $n$ acting on $c^\dagger_\uparrow c^\dagger_\downarrow |0\rangle$ gives the eigenvalue 2:
```wl
snegfermionoperators[c];
state = nc[c[CR, UP], c[CR, DO], VACUUM];
nc[number[c[]], state] == 2 state
```
---
Charge conservation. The hopping commutes with the total number operator; the pairing term $c^\dagger_\uparrow c^\dagger_\downarrow + \mathrm{h.c.}$ changes the charge by $\pm 2$:
```wl
snegfermionoperators[c];
Ntot = number[c[1]] + number[c[2]];
{komutator[hop[c[1], c[2]], Ntot], komutator[bcs[c[1]], Ntot]}
```
---
Grand-canonical Hamiltonian of a level with on-site energy $\epsilon$ and chemical potential $\mu$, and its spectrum in the subspaces of charge $Q$ and spin $S_z$:
```wl
snegfermionoperators[d];
snegrealconstants[eps, mu];
makematricesbzvc[(eps - mu) number[d[]], qszbasisvc[{d[]}]]
```
---
Thermal occupancy of a noninteracting level, $\langle n\rangle = 2 f(\epsilon) = 2/(e^{\epsilon/T} + 1)$, here for $\epsilon = 1$ and $T = 1/2$:
```wl
snegfermionoperators[d];
{thermalaverage[number[d[]], number[d[]], {d[]}, 1/2], N[2/(Exp[2] + 1)]}
```
---
Forms of the Hubbard interaction used in the Hubbard-Stratonovich decoupling in the charge and in the spin channel: $n_\uparrow n_\downarrow = \frac12 n^2 - \frac12 n = \frac12 n - \frac12 (n_\uparrow - n_\downarrow)^2$:
```wl
snegfermionoperators[c];
Expand[{1/2 pow[number[c[]], 2] - 1/2 number[c[]], 1/2 number[c[]] - 1/2 pow[number[c[], UP] - number[c[], DO], 2]} - hubbard[c[]]]
```

### Properties & Relations
$n_\sigma$ is a projector, $n_\sigma^2 = n_\sigma$, while $n^2 = n + 2 n_\uparrow n_\downarrow$:
```wl
snegfermionoperators[c];
{pow[number[c[], UP], 2] == number[c[], UP], pow[number[c[]], 2] == number[c[]] + 2 hubbard[c[]]}
```
---
Commutators with the creation and annihilation operators, $[n, c^\dagger_\sigma] = c^\dagger_\sigma$, $[n, c_\sigma] = -c_\sigma$:
```wl
{komutator[number[c[]], c[CR, UP]], komutator[number[c[]], c[AN, DO]]}
```
---
The number operator is Hermitian and commutes with the spin operators:
```wl
{conj[number[c[]]] == number[c[]], Expand[komutator[number[c[]], spinplus[c[]]]]}
```
---
Relation with the $z$ components of the spin and the isospin: $n_\uparrow - n_\downarrow = 2 S_z$ and $n - 1 = 2 I_z$:
```wl
{Expand[number[c[], UP] - number[c[], DO] - 2 spinz[c[]]], Expand[number[c[]] - 1 - 2 isospinz[c[]]]}
```
---
For bosons, $[b, b^\dagger b] = b$:
```wl
snegbosonoperators[b];
komutator[b[AN], number[b[]]]
```

### Possible Issues
The argument is the level, not an operator: `number[c[CR, 1, UP]]` treats `CR`, `1`, and `UP` as level indexes:
```wl
snegfermionoperators[c];
number[c[CR, 1, UP]]
```
Use `number[c[1], UP]` instead:
```wl
number[c[1], UP]
```
---
The operator must be declared. For an undeclared symbol, `number` remains unevaluated:
```wl
number[x[1]]
```
