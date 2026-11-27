---
keywords: single-particle basis, occupation number representation, Fock space, BASIS, exact diagonalization, mode ordering
see also: vc, vacuum, ap, op2ndx, ndx2op, goodopQ, basis, qszbasisvc, qsbasisvc, vc2ops
---
# makebasis

## Usage
- `makebasis[{c[i], d[j], ...}]` defines the single-particle basis for the occupation number representation from the levels `c[i]`, `d[j]`, ... and returns it.
- `makebasis[c[i], d[j], ...]` is equivalent.

## Details
- The levels are given without the creation/annihilation index and without the spin index: `c[i]` stands for the operators $c_{i\sigma}$, `c[]` is a level without further indexes.
- Every level is expanded into creation operators for all spin projections, from the highest to the lowest: `c[CR, i, UP]`, `c[CR, i, DO]` for spin-1/2 fermions, `c[CR, i, s]`, `c[CR, i, s-1]`, ..., `c[CR, i, -s]` for fermions declared with spin *s*, and `c[CR, i, 0]` for spinless fermions.
- The result is stored in the global variable `BASIS`. The position of an operator in `BASIS` is the position of its occupation number in the `vc` vectors; see `op2ndx` and `ndx2op`. For levels $c_1, c_2, \ldots$ the order is
$$c^\dagger_{1\uparrow}, c^\dagger_{1\downarrow}, c^\dagger_{2\uparrow}, c^\dagger_{2\downarrow}, \ldots,$$
and $\mathrm{vc}[n_1, n_2, \ldots] = (c^\dagger_{1\uparrow})^{n_1} (c^\dagger_{1\downarrow})^{n_2} \cdots |0\rangle$.
- The basis generators `qszbasisvc`, `qsbasisvc`, `qbasisvc`, `szbasisvc`, `sbasisvc`, `nonebasisvc`, and their operator-form versions call `makebasis` with their argument. Functions that build product bases from subsystems, such as `quickDBL`, call `makebasis` for the whole system at the end.
- `BASIS` is global: there is one occupation number representation at a time. `vc` vectors, `vacuum[]`, and the results of `ap` and `vc2ops` refer to the current `BASIS`.
- The levels can be nested in lists; the list is flattened. Duplicate levels are not removed.

## Developer Notes
- The expansion of a level into creation operators is performed by the internal function `mbfunc`, which is also used by `lrmap` to construct the mirror map of the left-right symmetry.
- NRG Ljubljana calls `makebasis[basisops]` for the operators of the impurity and the first Wilson chain sites and then works with `vacuum[]` and `vc` states.

## Examples
### Basic Examples
Single-particle basis of two spin-1/2 levels:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}]
```
```wl
InputForm[BASIS]
```
The vacuum and a two-electron state:
```wl
{vacuum[], ap[nc[c[CR, 1, UP], c[CR, 2, DO]], vacuum[]]} // InputForm
```

### Scope
Levels of different operators:
```wl
snegfermionoperators[d, c];
makebasis[d[], c[1]] // InputForm
```
---
Fermions with spin 1 have three components, ordered from $m = 1$ to $m = -1$:
```wl
snegfermionoperators[{f, 1}];
makebasis[{f[]}] // InputForm
```
---
Spinless fermions get the spin index 0:
```wl
snegspinlessfermionoperators[a];
makebasis[{a[1], a[2]}] // InputForm
```

### Applications
Full Fock space of a single level and the matrix of the Hubbard Hamiltonian $H = \epsilon n + U n_\uparrow n_\downarrow$ in it:
```wl
snegfermionoperators[d];
snegrealconstants[eps, U];
makebasis[{d[]}];
states = vc @@@ Tuples[{0, 1}, Length[BASIS]]
```
```wl
matrixrepresentationvc[eps number[d[]] + U hubbard[d[]], states] // MatrixForm
```

### Properties & Relations
The basis generators set `BASIS`:
```wl
snegfermionoperators[c];
qszbasisvc[{c[7]}];
BASIS
```
---
`op2ndx` and `ndx2op` map between operators and positions in `BASIS`:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
{op2ndx[c[AN, 2, UP]], ndx2op[2]} // InputForm
```

### Possible Issues
The argument is the level, not the operator. Here `CR` and `UP` are taken as level indexes:
```wl
snegfermionoperators[c];
makebasis[{c[CR, 1, UP]}] // InputForm
```
---
`BASIS` is global. States constructed for an earlier basis are reinterpreted after `makebasis` is called again:
```wl
snegfermionoperators[c, d];
makebasis[{c[1]}];
v = ap[c[CR, 1, UP], vacuum[]];
makebasis[{d[1]}];
vc2ops[v]
```
