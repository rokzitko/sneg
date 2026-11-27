---
keywords: creation operator representation, occupation number representation, conversion, Fock state
see also: vc2ops, ap, vc, vacuum, makebasis, bzop2bzvc
---
# ops2vc

## Usage
- `ops2vc[ops]` converts the state $\hat O |0\rangle$, given by the operator expression *ops* acting on the vacuum, into the occupation number representation.
- `ops2vc[ops, v]` gives $\hat O |v\rangle$ for a state *v* in the occupation number representation.

## Details
- `ops2vc[ops]` is `ap[ops, vacuum[]]`, and `ops2vc[ops, v]` is `ap[ops, v]`.
- The operators must belong to the single-particle basis `BASIS` defined by `makebasis`.
- `ops2vc` is the inverse of `vc2ops`. `bzop2bzvc` converts a whole basis.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
ops2vc[nc[c[CR, 2, DO], c[CR, 1, UP]]] // InputForm
```
The two-site singlet $(c^\dagger_{1\uparrow} c^\dagger_{2\downarrow} - c^\dagger_{1\downarrow} c^\dagger_{2\uparrow})|0\rangle/\sqrt 2$:
```wl
s = (nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];
ops2vc[s] // InputForm
```

### Scope
Application to a given state:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
ops2vc[c[CR, 2, UP], vc[1, 0, 0, 0]] // InputForm
```

### Properties & Relations
`vc2ops` is the inverse conversion:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vc2ops[ops2vc[nc[c[CR, 2, DO], c[CR, 1, UP]]]]
```
