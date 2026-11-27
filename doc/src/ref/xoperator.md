---
keywords: Hubbard X-operators, Hubbard operators, projection operators, atomic limit, t-J model, Hubbard atom, ket-bra operators
see also: xtofermions, fermionstox, xtospin, snegxoperators, projector, hubbard
related links: [J. Hubbard, Proc. R. Soc. London A 285, 542 (1965)](https://doi.org/10.1098/rspa.1965.0124)
---
# xoperator

## Usage
- `xoperator[c[j], a, b]` gives the Hubbard X-operator $X^{ab}_j = |a\rangle\langle b|$ of the Hubbard atom of the spin-1/2 fermion level `c[j]`, expressed with fermion operators.
- `xoperator[c, a, b]` is `xoperator[c[], a, b]`.

## Details
- The states of the Hubbard atom are labeled by the strings `"0"`, `"UP"`, `"DO"`, `"2"`:
  $$|0\rangle, \quad |{\uparrow}\rangle = c^\dagger_\uparrow |0\rangle, \quad |{\downarrow}\rangle = c^\dagger_\downarrow |0\rangle, \quad |2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow |0\rangle.$$
  The sign convention of $|2\rangle$ determines the signs of the X-operators that involve it.
- The operator is constructed as $X^{ab} = C_a\, (1 - n_\uparrow)(1 - n_\downarrow)\, C_b^\dagger$ with $C_0 = 1$, $C_\uparrow = c^\dagger_\uparrow$, $C_\downarrow = c^\dagger_\downarrow$, $C_2 = c^\dagger_\uparrow c^\dagger_\downarrow$, and normal ordered. For example $X^{0\uparrow} = c_\uparrow (1 - n_\downarrow)$, $X^{\uparrow\uparrow} = n_\uparrow (1 - n_\downarrow)$, $X^{\uparrow\downarrow} = c^\dagger_\uparrow c_\downarrow = S^+$, $X^{22} = n_\uparrow n_\downarrow$.
- The X-operators satisfy $X^{ab} X^{cd} = \delta_{bc} X^{ad}$ and the completeness relation $\sum_a X^{aa} = 1$. $X^{ab}$ is fermionic if the states $a$ and $b$ differ in the particle number by one.
- An invalid state label gives the message `xoperator::state` and returns `$Failed`.
- Abstract X-operators with the algebra built in are declared with `snegxoperators`. `xtofermions` and `fermionstox` convert between the abstract X-operators of the Hubbard atom and fermions, and `xtospin` converts the X-operators of singly occupied sites to spin operators.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
xoperator[c[1], "0", "UP"]
```
All 16 X-operators of the Hubbard atom:
```wl
st = {"0", "UP", "DO", "2"};
Table[xoperator[c[], a, b], {a, st}, {b, st}] // MatrixForm
```

### Scope
A level without indexes, given by the bare symbol:
```wl
snegfermionoperators[d];
xoperator[d, "UP", "DO"] == spinplus[d[]]
```

### Applications
Hubbard operators split the electron operator into the parts that change the occupancy between 0 and 1, and between 1 and 2: $c^\dagger_\uparrow = X^{\uparrow 0} + X^{2\downarrow}$:
```wl
snegfermionoperators[c];
Expand[xoperator[c[], "UP", "0"] + xoperator[c[], "2", "DO"] - c[CR, UP]]
```
In the $U \to \infty$ limit, only $X^{\uparrow 0}$ survives (projected hopping of the t-J model).

### Properties & Relations
Completeness, $\sum_a X^{aa} = 1$:
```wl
snegfermionoperators[c];
Expand[Total[xoperator[c[], #, #] & /@ {"0", "UP", "DO", "2"}]]
```
---
Multiplication rule $X^{0\uparrow} X^{\uparrow 2} = X^{02}$:
```wl
snegfermionoperators[c];
Expand[nc[xoperator[c[], "0", "UP"], xoperator[c[], "UP", "2"]] - xoperator[c[], "0", "2"]]
```
---
The diagonal X-operators are the projectors of `projector`:
```wl
snegfermionoperators[c];
Expand[{xoperator[c[], "0", "0"] - projector0[c[]], xoperator[c[], "UP", "UP"] - projectorUP[c[]], xoperator[c[], "2", "2"] - projector2[c[]]}]
```
---
$X^{ab}$ applied to $|b\rangle$ gives $|a\rangle$; with the convention $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow |0\rangle$:
```wl
snegfermionoperators[c];
nc[xoperator[c[], "2", "DO"], c[CR, DO], VACUUM]
```

### Possible Issues
The state labels are strings:
```wl
snegfermionoperators[c];
xoperator[c[], "UP", UP]
```
