---
keywords: scalar product, inner product, overlap, occupation number representation, sesquilinear form, orthonormality
see also: scalarproduct, scalarproductop, normvc, braketvc, expvvc, decomposevc, orthogvc, vc, braketrule
---
# scalarproductvc

## Usage
- `scalarproductvc[a, b]` gives the scalar product $\langle a | b \rangle$ of the states *a* and *b* in the occupation number representation.

## Details
- *a* and *b* are `vc` vectors or linear combinations of them. The `vc` vectors are orthonormal: `scalarproductvc[vc[...], vc[...]]` is 1 if both vectors are identical and 0 otherwise.
- Physics convention: the scalar product is conjugate-linear in the first argument and linear in the second, $\langle \alpha a | \beta b\rangle = \alpha^* \beta \langle a | b\rangle$. Complex constants declared with `snegcomplexconstants` are conjugated; real constants and numbers are handled accordingly.
- For states with kets, `vc[..., ket[k1]]` and `vc[..., ket[k2]]`, the fermionic overlap is multiplied by `braketrule[bra[k1], ket[k2]]`.
- In an `nc` product, `nc[conj[v1], v2]` with `vc` vectors *v1* and *v2* is replaced by `scalarproductvc[v1, v2]`.
- `scalarproductvc` is `Listable`. `scalarproduct` is an alias.
- The vectors are compared literally; symbolic occupation numbers are not supported.
- Undeclared symbols in the coefficients are treated as operators, and the scalar product then remains partly unevaluated (see Possible Issues).

## Developer Notes
- `scalarproductvc` is declared with `snegsesquilinearoperator`, which implements the conjugate-linearity in the first argument and the linearity in the second.
- The corresponding function for the creation operator representation is `scalarproductop[a, b]`, computed as the vacuum expectation value of `nc[conj[a], b]` with Wick's theorem.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
makebasis[{d[]}];
{scalarproductvc[vc[1, 0], vc[1, 0]], scalarproductvc[vc[1, 0], vc[0, 1]]}
```
The first argument is conjugated:
```wl
psi = (vc[1, 0] + I vc[0, 1])/Sqrt[2];
{scalarproductvc[psi, vc[0, 1]], scalarproductvc[vc[0, 1], psi]}
```

### Scope
Declared constants:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
snegcomplexconstants[z];
snegrealconstants[x];
scalarproductvc[z vc[1, 0] + x vc[0, 1], vc[1, 0] + vc[0, 1]]
```
---
States with kets:
```wl
{scalarproductvc[vc[1, 0, ket[1/2]], vc[1, 0, ket[1/2]]], scalarproductvc[vc[1, 0, ket[1/2]], vc[1, 0, ket[-1/2]]]}
```
---
In `nc` products:
```wl
nc[conj[vc[1, 1]], vc[1, 1]]
```
---
`scalarproductvc` is `Listable`:
```wl
scalarproductvc[{vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}, vc[0, 1]]
```

### Applications
Orthonormality of the $(Q, S)$ basis of two sites:
```wl
snegfermionoperators[c];
states = Flatten[Last /@ qsbasisvc[{c[1], c[2]}]];
Outer[scalarproductvc, states, states] == IdentityMatrix[Length[states]]
```
---
Overlap between the ground state of the Hubbard dimer in the limit $U \to \infty$, $(|\uparrow,\downarrow\rangle - |\downarrow,\uparrow\rangle)/\sqrt 2$, and the ground state at $U = 0$, the doubly occupied bonding orbital:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
heitlerlondon = (vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];
bonding = ap[nc[c[CR, 1, UP] + c[CR, 2, UP], c[CR, 1, DO] + c[CR, 2, DO]]/2, vacuum[]];
scalarproductvc[heitlerlondon, bonding]
```

### Properties & Relations
`normvc` is the square root of the scalar product of a state with itself:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
v = 3 vc[1, 0] + 4 I vc[0, 1];
{scalarproductvc[v, v], normvc[v]}
```
---
`scalarproductop` gives the same result in the creation operator representation:
```wl
a = vc[1, 0] + I vc[1, 1];
b = vc[1, 1];
scalarproductvc[a, b] == scalarproductop[vc2ops[a], vc2ops[b]]
```

### Possible Issues
Coefficients must be declared constants:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
scalarproductvc[y vc[1, 0], vc[1, 0]]
```
```wl
snegrealconstants[y];
scalarproductvc[y vc[1, 0], vc[1, 0]]
```
