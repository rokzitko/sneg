---
keywords: basis transformation, unitary transformation, change of basis, bonding orbital, antibonding orbital, molecular orbitals, canonical transformation
see also: conj, hop, transformtoLR, bogoliubovrules
---
# snegold2newrules

## Usage
- `snegold2newrules[{o1, o2, ...}, {n1, n2, ...}, {r1, r2, ...}]` gives the rules that express the old levels *o1*, *o2*, ... in terms of the new levels *n1*, *n2*, ..., where the new levels are defined by the linear combinations *r1*, *r2*, ... of the old levels.

## Details
- The levels are given without the creation/annihilation index and without the spin index, e.g. `c[1]` or `e[]`. The definitions $n_i = \sum_j M_{ij} o_j$ are given as linear combinations of the old levels.
- The transformation matrix $M$ must be unitary; then $o_j = \sum_i (M^{-1})_{ji} n_i$, and the result is the list of rules `o_j -> ...`.
- If the numbers of old and new levels differ, the text "Mismatching lengths." is printed and `Null` is returned. If $M$ is not unitary, the text "The transformation rule is not unitary." is printed and `Null` is returned. For symbolic matrices, the unitarity check is not decided and is skipped.
- The rules act on the levels. To transform operator expressions, they must be extended to the operators `c[CR, i, σ]` and `c[AN, i, σ]`; for a real matrix $M$, the same coefficients apply to creation and annihilation operators (see the examples). For a complex $M$, the coefficients of the creation operators are complex conjugated.

## Examples
### Basic Examples
Bonding and antibonding orbitals of two sites, $e = (c_1 + c_2)/\sqrt2$, $o = (c_1 - c_2)/\sqrt2$:
```wl
snegfermionoperators[c, e, o];
rules = snegold2newrules[{c[1], c[2]}, {e[], o[]}, {(c[1] + c[2])/Sqrt[2], (c[1] - c[2])/Sqrt[2]}]
```
Rules for the operators with the creation/annihilation and spin indexes (real transformation):
```wl
oprules = c[t_, i_, s_] :> ((c[i] /. rules) /. op_?operatorQ[] :> op[t, s]);
c[CR, 1, UP] /. oprules
```

### Applications
The hopping between two sites is diagonal in the basis of bonding and antibonding orbitals, $c^\dagger_1 c_2 + \mathrm{h.c.} = n_e - n_o$:
```wl
snegfermionoperators[c, e, o];
rules = snegold2newrules[{c[1], c[2]}, {e[], o[]}, {(c[1] + c[2])/Sqrt[2], (c[1] - c[2])/Sqrt[2]}];
oprules = c[t_, i_, s_] :> ((c[i] /. rules) /. op_?operatorQ[] :> op[t, s]);
Expand[hop[c[1], c[2]] /. oprules] == Expand[number[e[]] - number[o[]]]
```
The total charge is invariant, while the Hubbard interaction contains pair-hopping and exchange terms in the new basis:
```wl
Expand[(number[c[1]] + number[c[2]]) /. oprules]
```
```wl
Expand[(hubbard[c[1]] + hubbard[c[2]]) /. oprules]
```
---
A rotation by a symbolic angle; `Simplify` removes the denominators $\cos^2\theta + \sin^2\theta$:
```wl
snegfermionoperators[c, e, o];
snegrealconstants[θ];
snegold2newrules[{c[1], c[2]}, {e[], o[]}, {Cos[θ] c[1] + Sin[θ] c[2], -Sin[θ] c[1] + Cos[θ] c[2]}] // Simplify
```

### Possible Issues
A transformation that is not unitary is rejected:
```wl
snegfermionoperators[c, e, o];
snegold2newrules[{c[1], c[2]}, {e[], o[]}, {c[1] + c[2], c[1] - c[2]}]
```
