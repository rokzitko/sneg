---
keywords: expectation value, diagonal matrix element, occupation number representation, correlation function
see also: braketvc, expvop, scalarproductvc, matrixrepresentationvc, ap
---
# expvvc

## Usage
- `expvvc[op, a]` gives the diagonal matrix element $\langle a | \hat O | a \rangle$ of the operator *op* in the state *a* in the occupation number representation.

## Details
- `expvvc[op, a]` is `braketvc[a, op, a]`, i.e. `scalarproductvc[a, ap[op, a]]`.
- The result is not divided by $\langle a | a \rangle$: for a normalized expectation value, the state must be normalized.
- `expvvc` is `Listable` in both arguments. The corresponding function for the creation operator representation is `expvop`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
singlet = (vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];
expvvc[spinspin[c[1], c[2]], singlet]
```
For the triplet, $\langle \mathbf S_1 \cdot \mathbf S_2 \rangle = 1/4$:
```wl
expvvc[spinspin[c[1], c[2]], vc[1, 0, 1, 0]]
```

### Scope
Several operators at once:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
psi = (vc[1, 1, 0, 0] + vc[0, 0, 1, 1])/Sqrt[2];
expvvc[{number[c[1]], hubbard[c[1]], nc[number[c[1]], number[c[2]]]}, psi]
```

### Applications
Energy and double occupancy $\langle n_{1\uparrow} n_{1\downarrow}\rangle$ in the ground state of the half-filled Hubbard dimer for $t = 1$, $U = 4$. The ground state is the singlet with $E_0 = (U - \sqrt{U^2 + 16 t^2})/2 = 2 - 2\sqrt 2$:
```wl
snegfermionoperators[c];
bz = qsbasisvc[{c[1], c[2]}];
states = Last[First[Select[bz, First[#] == {0, 0} &]]];
H = -hop[c[1], c[2]] + 4 Total[hubbard[{c[1], c[2]}]];
{vals, vecs} = Eigensystem[matrixrepresentationvc[H, states]];
psi = Normalize[vecs[[First[Ordering[N[vals]]]]]] . states;
Simplify[expvvc[{H, hubbard[c[1]]}, psi]]
```
The double occupancy agrees with $\frac14 (1 - U/\sqrt{U^2 + 16 t^2})$:
```wl
N[{1/(8 + 4 Sqrt[2]), (1 - 4/Sqrt[32])/4}]
```

### Properties & Relations
`expvvc[op, a]` equals `braketvc[a, op, a]`:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
a = (vc[1, 0] + vc[1, 1])/Sqrt[2];
expvvc[number[d[]], a] == braketvc[a, number[d[]], a]
```

### Possible Issues
The state is not normalized automatically:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
expvvc[number[d[]], 2 vc[1, 0]]
```
