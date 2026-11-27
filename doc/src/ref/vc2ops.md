---
keywords: occupation number representation, creation operator representation, conversion, Fock state
see also: ops2vc, vc, ap, makebasis, vacuum, bzvc2bzop, bzop2bzvc
---
# vc2ops

## Usage
- `vc2ops[expr]` converts the states `vc[...]` in *expr* from the occupation number representation into creation operator strings acting on the vacuum.

## Details
- `vc[n1, n2, ...]` is converted into `nc[a1, a2, ...]` of the creation operators $a^\dagger_k$ of `BASIS` with $n_k = 1$, in the order of `BASIS`: $\mathrm{vc}[n_1, n_2, \ldots] \to (a^\dagger_1)^{n_1} (a^\dagger_2)^{n_2} \cdots$. The product is then brought to the normal order of `nc`, which can change the sign.
- The vacuum `vacuum[]` is converted into 1.
- A trailing ket is kept: `vc[n1, ..., ket[k]]` gives `nc[..., ket[k]]`.
- `vc2ops` is linear and `Listable`. `bzvc2bzop` converts a whole basis.
- If the length of the vector differs from the length of `BASIS`, the message `vc2ops::incom` is issued and `Infinity` is returned.
- The inverse conversion is `ops2vc` (or `ap[ops, vacuum[]]`): `ops2vc[vc2ops[v]]` is *v*.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vc2ops[vc[1, 0, 0, 1]]
```
The normal order of `nc` puts $c^\dagger_{1\downarrow}$ to the left of $c^\dagger_{1\uparrow}$, which gives a sign:
```wl
vc2ops[vc[1, 1, 0, 0]]
```

### Scope
Linear combinations; the singlet state of two sites:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vc2ops[(vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2]]
```
---
The vacuum corresponds to the identity:
```wl
vc2ops[vacuum[]]
```
---
States with kets:
```wl
vc2ops[vc[0, 1, 0, 0, ket[1/2]]]
```
---
All states of one level:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
vc2ops[vc @@@ Tuples[{0, 1}, 2]]
```

### Properties & Relations
`ops2vc` is the inverse conversion:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
v = vc[1, 1, 0, 1];
ops2vc[vc2ops[v]] === v
```

### Possible Issues
The vector must have the length of `BASIS`:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vc2ops[vc[1, 0]]
```
