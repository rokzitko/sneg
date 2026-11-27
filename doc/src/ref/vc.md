---
keywords: occupation number representation, Fock state, many-particle state, state vector, Slater determinant, exact diagonalization
see also: makebasis, vacuum, ap, vc2ops, ops2vc, op2ndx, scalarproductvc, qszbasisvc, qsbasisvc, vccolor, ket
---
# vc

## Usage
- `vc[n1, n2, ..., nN]` represents the Fock state with occupation numbers $n_k \in \{0, 1\}$ of the single-particle states in the list `BASIS` defined by `makebasis`.
- `vc[n1, ..., nN, ket[...]]` is the product of a fermionic Fock state with a Dirac ket of additional degrees of freedom (spins, phonons).

## Details
- `makebasis[{c[1], c[2], ...}]` sets the global list `BASIS` of single-particle creation operators. For spin-1/2 levels the order is $c^\dagger_{1\uparrow}, c^\dagger_{1\downarrow}, c^\dagger_{2\uparrow}, c^\dagger_{2\downarrow}, \ldots$; `op2ndx` and `ndx2op` convert between operators and positions.
- Sign convention: with $a^\dagger_k$ the k-th element of `BASIS`,
$$\mathrm{vc}[n_1, n_2, \ldots, n_N] = (a^\dagger_1)^{n_1} (a^\dagger_2)^{n_2} \cdots (a^\dagger_N)^{n_N} |0\rangle,$$
i.e., the creation operators act in the order of `BASIS`, with the operator of the lowest index on the left. Applying $a^\dagger_k$ or $a_k$ to this state gives the fermionic sign $(-1)^{\sum_{l<k} n_l}$ (see `ap`).
- `vc` states form an orthonormal basis: `scalarproductvc[vc[...], vc[...]]` is 1 for identical vectors and 0 otherwise. General states are linear combinations `x vc[...] + y vc[...]` with scalar coefficients.
- Operators act on `vc` states with `ap[op, v]`. In a product `nc[a, b, ..., v]` that ends with a `vc` state, `ap` is called automatically.
- `vacuum[]` is `vc[0, 0, ..., 0]` with the length of `BASIS`. `vc2ops` converts a `vc` state into the equivalent string of creation operators (the creation operator representation); `ops2vc` converts back.
- A `ket` as the last element describes non-fermionic degrees of freedom. Kets are appended by `ap[ket[...], v]`, bras act on them by `braketrule`, and `scalarproductvc` multiplies the fermionic overlap with the braket of the kets.
- `vc` has the attributes `NHoldAll` and `Protected`: `N` does not change the occupation numbers.
- With `PrettyOutput` (the default), `vc` states are displayed as rows of filled (occupied) and empty (unoccupied) rectangles, red for spin up and blue for spin down; see `vccolor`.
- The `vc` vectors are meaningful only together with the `BASIS` for which they were constructed. Basis generators such as `qszbasisvc` and `qsbasisvc` call `makebasis` and thereby redefine `BASIS`.

## Developer Notes
- `vc` has no definitions apart from formatting; all rules are attached to `ap`, `scalarproductvc`, `vc2ops`, and `nc` (`nc[a___, v_vc] := ap[nc[a], v]` and `nc[a___, conj[v1_vc], v2_vc, b___]`, which evaluates the scalar product).
- The `vc` representation is used by the NRG Ljubljana initialization (`nrginit`) for all matrix elements: it is much faster than the creation operator representation, since applying an operator to a `vc` state does not require normal ordering.

## Examples
### Basic Examples
Single-particle basis of one spin-1/2 level `d[]`:
```wl
snegfermionoperators[d];
makebasis[{d[]}]
```
The four Fock states of the level:
```wl
states = {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}
```
```wl
InputForm[states]
```
The doubly occupied state is $d^\dagger_\uparrow d^\dagger_\downarrow |0\rangle$:
```wl
ap[nc[d[CR, UP], d[CR, DO]], vacuum[]]
```
```wl
ap[nc[d[CR, DO], d[CR, UP]], vacuum[]] // InputForm
```

### Scope
Two levels: `BASIS` has four elements, and the vectors have four components:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
BASIS
```
The fermionic sign: $c^\dagger_{1\downarrow}$ acting on $c^\dagger_{1\uparrow} c^\dagger_{2\uparrow} |0\rangle$ passes one occupied state:
```wl
ap[c[CR, 1, DO], vc[1, 0, 1, 0]] // InputForm
```
---
Linear combinations of `vc` states; the singlet state of two sites:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
singlet = (vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];
{normvc[singlet], expvvc[spinss[c[1]] + spinss[c[2]] + 2 spinspin[c[1], c[2]], singlet]}
```
---
A `vc` state with a ket for an additional spin 1/2:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
v = ap[ket[1/2], vc[1, 0]]
```
```wl
InputForm[v]
```

### Properties & Relations
`vc` states are orthonormal:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
states = {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]};
Outer[scalarproductvc, states, states]
```
---
Conversion to the creation operator representation and back:
```wl
vc2ops[vc[1, 1]]
```
```wl
ops2vc[vc2ops[vc[1, 1]]] // InputForm
```
---
`N` does not change the occupation numbers:
```wl
N[vc[1, 0]/Sqrt[2]] // InputForm
```

### Possible Issues
A `vc` vector refers to the current `BASIS`. After another basis is defined, the same vector describes a different state:
```wl
snegfermionoperators[c, d];
makebasis[{c[]}];
vc2ops[vc[1, 0]]
```
```wl
makebasis[{d[]}];
vc2ops[vc[1, 0]]
```
---
Vectors of the wrong length are rejected by `vc2ops`:
```wl
vc2ops[vc[1, 0, 0]]
```
