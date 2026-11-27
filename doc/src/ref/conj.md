---
keywords: Hermitian conjugation, adjoint, dagger, complex conjugation, Hermitian operator, bra, ket
see also: nc, snegrealconstants, snegcomplexconstants, sneggrassmanconstants, VACUUM, bra, ket, komutator
---
# conj

## Usage
- `conj[expr]` gives the Hermitian conjugate $\mathit{expr}^\dagger$ of an operator expression *expr*.

## Details
- `conj` is antilinear and reverses the order of products: $(z A B)^\dagger = z^* B^\dagger A^\dagger$, and `conj[a + b]` is `conj[a] + conj[b]`.
- Numbers and declared constants are complex conjugated: real constants (`snegrealconstants`) are unchanged, and complex constants (`snegcomplexconstants`) give `Conjugate[z]`. Functions declared with `snegrealfunctions` are real.
- Fermion and boson operators: $c[\mathrm{CR}, \ldots] \leftrightarrow c[\mathrm{AN}, \ldots]$, with all other indexes unchanged. Majorana operators are self-adjoint.
- Spin operators: $S^x, S^y, S^z$ are self-adjoint and $(S^\pm)^\dagger = S^\mp$. X-operators: $(X^{ab})^\dagger = X^{ba}$.
- States: `conj[ket[i]]` is `bra[i]` and vice versa; `conj[VACUUM]` represents the bra $\langle 0|$.
- Grassmann numbers $z$ (`sneggrassmanconstants`) are conjugated to the independent Grassmann numbers `conj[z]`.
- `conj[conj[a]]` is *a*. `conj` maps over sums `sum[a, {k}]`, exponentials (`conj[Exp[a]]` is `Exp[conj[a]]`), and rules.
- `conj` is `Listable`.
- Symbols that are not declared as constants are treated as operators; `conj[u]` is then returned unevaluated. Indexes declared with `snegfreeindexes` are not assumed to be real; `conj[k]` gives `Conjugate[k]`.

## Examples
### Basic Examples
Hermitian conjugate of a product with a complex coefficient:
```wl
snegfermionoperators[c];
snegcomplexconstants[z];
conj[z nc[c[CR, 1, UP], c[AN, 2, DO]]]
```
---
The hopping operator is Hermitian:
```wl
snegfermionoperators[c];
Expand[conj[hop[c[1], c[2]]] - hop[c[1], c[2]]]
```

### Scope
Real and complex constants, numbers:
```wl
snegrealconstants[t];
snegcomplexconstants[z];
conj[{t, z, 2 + 3 I, Exp[I t]}]
```
---
Boson operators and Majorana operators:
```wl
snegbosonoperators[a];
snegmajoranaoperators[γ];
conj[{a[CR], nc[a[CR], a[AN], a[AN]], γ[1]}]
```
---
Spin operators, $(S^+)^\dagger = S^-$:
```wl
snegspinoperators[{S, 1/2}];
conj[{S[1, 1], S[1, 4], nc[S[1, 4], S[2, 5]]}]
```
---
Bras, kets, and the vacuum state:
```wl
snegfermionoperators[c];
conj[{ket[1], nc[c[CR, 1, UP], VACUUM]}]
```
---
Sums over indexes:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegcomplexfunctions[v];
conj[sum[v[k] nc[c[CR, k, UP], c[AN, 0, UP]], {k}]]
```
---
Grassmann numbers:
```wl
sneggrassmanconstants[g];
snegfermionoperators[c];
{conj[nc[g, c[CR, UP]]], conj[conj[g]]}
```
---
Rules:
```wl
snegfermionoperators[c, d];
conj[c[CR, 1, UP] -> d[CR, 1, UP]]
```

### Applications
Hermiticity check of the Anderson impurity Hamiltonian with complex hybridization $V$:
```wl
snegfermionoperators[c, d];
snegrealconstants[ed, U];
snegcomplexconstants[V];
H = ed number[d[]] + U hubbard[d[]] + Sum[V nc[c[CR, σ], d[AN, σ]] + Conjugate[V] nc[d[CR, σ], c[AN, σ]], {σ, {UP, DO}}];
Expand[conj[H] - H]
```
---
Construction of a Hermitian operator: the BCS pairing term $\Delta c^\dagger_\uparrow c^\dagger_\downarrow + \mathrm{h.c.}$:
```wl
snegfermionoperators[c];
snegcomplexconstants[Δ];
pair = Δ nc[c[CR, UP], c[CR, DO]];
pair + conj[pair]
```
---
The norm of a state $|\psi\rangle = \psi |0\rangle$ in the creation-operator representation is $\langle 0| \psi^\dagger \psi |0\rangle$:
```wl
snegfermionoperators[c];
snegrealconstants[α, β];
psi = α c[CR, 1, UP] + β nc[c[CR, 1, UP], c[CR, 1, DO]];
vev[nc[conj[psi], psi]]
```

### Properties & Relations
Repeated conjugation is the identity:
```wl
snegfermionoperators[c];
snegcomplexconstants[z];
x = z nc[c[CR, 1, UP], c[AN, 2, DO]];
conj[conj[x]] == x
```
---
$(A B)^\dagger = B^\dagger A^\dagger$:
```wl
snegfermionoperators[c];
A = c[CR, 1, UP] + c[AN, 2, UP];
B = nc[c[CR, 3, DO], c[AN, 1, UP]];
Expand[conj[nc[A, B]] - nc[conj[B], conj[A]]]
```

### Possible Issues
Undeclared symbols are treated as operators and are not conjugated:
```wl
snegfermionoperators[c];
conj[w c[CR, 1, UP]]
```
```wl
snegrealconstants[w];
conj[w c[CR, 1, UP]]
```
---
Summation indexes are not assumed to be real:
```wl
snegfreeindexes[k];
conj[k]
```
