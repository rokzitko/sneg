---
keywords: apply operator, operator acting on state, occupation number representation, fermionic sign, Fock state, exact diagonalization
see also: vc, makebasis, vacuum, nc, vc2ops, ops2vc, braketvc, scalarproductvc, matrixrepresentationvc, makematricesbzvc
---
# ap

## Usage
- `ap[op, v]` applies the operator *op* to the state *v* in the occupation number representation.
- `ap[op1, op2, ..., v]` applies the product $\hat O_1 \hat O_2 \cdots$ to *v*.
- `ap[op, VACUUM]` applies *op* to `vacuum[]`.

## Details
- *v* is a `vc` vector or a linear combination of `vc` vectors. *op* can be a single fermion operator, a product `nc[...]`, a sum of terms with scalar coefficients, or a ket-bra operator acting on the kets in `vc[..., ket[...]]`.
- For the k-th operator $a_k$ of `BASIS` and its adjoint, the rules are
$$a^\dagger_k |\ldots n_k \ldots\rangle = (-1)^{\sum_{l<k} n_l} \delta_{n_k,0} |\ldots 1 \ldots\rangle, \quad a_k |\ldots n_k \ldots\rangle = (-1)^{\sum_{l<k} n_l} \delta_{n_k,1} |\ldots 0 \ldots\rangle,$$
in agreement with the convention $\mathrm{vc}[n_1, \ldots] = (a^\dagger_1)^{n_1} (a^\dagger_2)^{n_2} \cdots |0\rangle$ (see `vc`). The implementation does not depend on the spin of the fermions.
- Products act from the right: `ap[nc[A, B], v]` is `ap[A, ap[B, v]]`.
- `ap` is linear in all arguments: sums are distributed and scalar factors are pulled out. Scalars are numbers and declared constants (`snegrealconstants`, `snegcomplexconstants`); undeclared symbols are treated as operators (see Possible Issues).
- If a product `nc[..., v]` ends with a `vc` vector, `ap` is called automatically: `nc[op, v]` is `ap[op, v]`.
- Kets: `ap[ket[k], v]` appends the ket to *v*, `vc[..., ket[k]]`; kets with complementary `Null` slots are combined (`ket[a, Null]` and `ket[Null, b]` give `ket[a, b]`). A bra acting on the ket of *v* gives `braketrule[bra[...], ket[...]]`. Ket-bra operators such as `nc[ket[i], bra[j]]` thus act on the kets.
- `ap[0, v]` and `ap[op, 0]` are 0; `ap[v]` is *v*.
- `ap` is `Listable`.
- An operator whose level is not in `BASIS` is not applied: `ap` returns unevaluated. Products that contain such operators lead to infinite recursion; the evaluation is aborted with `$RecursionLimit`. Always define the basis for all levels that occur in the operators.
- `ap` is applicable to fermion operators and to bra-ket operators. Spin operators `S[...]` and boson operators are not applied; use kets for such degrees of freedom.

## Developer Notes
- The single-operator rule uses `op2ndx` to find the position of the operator in `BASIS` and counts the occupied positions to its left. `ap[x:nc[...], v]` applies the factors from right to left with `Fold`.
- `ap` is declared with `sneglinearoperatorWithLast`, which makes it linear in all arguments.
- The rule `ap[a___, ap[b___, c_]] := ap[nc[a, b], c]` combines nested unevaluated applications; together with an operator outside `BASIS`, this rule and `nc[a___, v_vc] := ap[nc[a], v]` cause the infinite recursion mentioned above.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vac = vacuum[];
ap[c[CR, 1, UP], vac] // InputForm
```
Annihilation operators annihilate the vacuum:
```wl
ap[c[AN, 1, UP], vac]
```
Two-particle state $c^\dagger_{2\uparrow} c^\dagger_{1\downarrow} |0\rangle = -c^\dagger_{1\downarrow} c^\dagger_{2\uparrow} |0\rangle$:
```wl
ap[nc[c[CR, 2, UP], c[CR, 1, DO]], vac] // InputForm
```

### Scope
Number and spin operators:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
v = vc[1, 1, 1, 0];
{ap[number[c[1]], v], ap[spinz[c[2]], v]} // InputForm
```
---
Hopping $\sum_\sigma (c^\dagger_{1\sigma} c_{2\sigma} + \mathrm{h.c.})$ acting on a state with one electron on each site:
```wl
ap[hop[c[1], c[2]], vc[1, 0, 0, 1]] // InputForm
```
---
Linear combinations with declared constants:
```wl
snegrealconstants[t, U];
ap[t c[CR, 1, UP] + U c[CR, 2, DO], vacuum[]] // InputForm
```
---
A product ending with a `vc` vector is evaluated automatically:
```wl
nc[c[AN, 1, UP], c[CR, 1, UP], vacuum[]] // InputForm
```
---
`ap` is `Listable`:
```wl
ap[{c[CR, 1, UP], c[CR, 1, DO], c[AN, 1, UP]}, vacuum[]] // InputForm
```
---
Applying an operator to the vacuum given as `VACUUM`:
```wl
ap[c[CR, 2, DO], VACUUM] // InputForm
```
---
Ket-bra operators act on the kets. Spin raising operator $S^+ = |\uparrow\rangle\langle\downarrow|$ of an additional spin 1/2:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
v = ap[ket[-1/2], vc[1, 0]]
```
```wl
ap[nc[ket[1/2], bra[-1/2]], v] // InputForm
```

### Applications
The singlet state of the Hubbard dimer at $U = 0$ is an eigenstate of the hopping term $H = -t \sum_\sigma (c^\dagger_{1\sigma} c_{2\sigma} + \mathrm{h.c.})$ with eigenvalue $-2t$. The bonding orbital is $(c^\dagger_{1\sigma} + c^\dagger_{2\sigma})/\sqrt 2$:
```wl
snegfermionoperators[c];
snegrealconstants[t];
makebasis[{c[1], c[2]}];
H = -t hop[c[1], c[2]];
psi = ap[nc[(c[CR, 1, UP] + c[CR, 2, UP])/Sqrt[2], (c[CR, 1, DO] + c[CR, 2, DO])/Sqrt[2]], vacuum[]];
Expand[ap[H, psi] + 2 t psi]
```
---
The Hubbard interaction $U n_{\uparrow} n_{\downarrow}$ is diagonal in the occupation number basis:
```wl
snegfermionoperators[d];
snegrealconstants[U];
makebasis[{d[]}];
ap[U hubbard[d[]], {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}] // InputForm
```

### Properties & Relations
The canonical anticommutation relation $\{c, c^\dagger\} = 1$ holds on every state:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
v = vc[0, 1, 1, 0];
ap[nc[c[AN, 1, UP], c[CR, 1, UP]], v] + ap[nc[c[CR, 1, UP], c[AN, 1, UP]], v] === v
```
---
`ap` agrees with the creation operator representation: converting with `vc2ops`, multiplying with `nc`, and converting back gives the same state:
```wl
op = c[CR, 2, UP] + c[AN, 1, DO];
v = vc[1, 1, 0, 0];
Expand[ap[op, v] - ops2vc[nc[op, vc2ops[v]]]]
```

### Possible Issues
Undeclared symbols are treated as operators, and the result is left unevaluated:
```wl
snegfermionoperators[c];
makebasis[{c[1]}];
ap[x c[CR, 1, UP], vacuum[]]
```
```wl
snegrealconstants[x];
ap[x c[CR, 1, UP], vacuum[]] // InputForm
```
---
Operators of levels that are not in `BASIS` are not applied:
```wl
snegfermionoperators[c, e];
makebasis[{c[1]}];
ap[e[CR, UP], vacuum[]]
```
---
Spinless fermions carry the spin index 0 in `BASIS`, as in the operators generated by `number` and `hop`. An operator written without this index is not recognized:
```wl
snegspinlessfermionoperators[f];
makebasis[{f[1]}]
```
```wl
{ap[f[CR, 1], vacuum[]], ap[f[CR, 1, 0], vacuum[]]}
```
