---
keywords: Dirac notation, ket, bra-ket, state vector, spin states, phonon states, tensor product
see also: bra, braketrule, ketbratensorproduct, nc, conj, ap, vc, spinket, spinbasis, phononbasis
---
# ket

## Usage
- `ket[i]` represents the Dirac ket $|i\rangle$.
- `ket[i, j, ...]` represents the product state $|i, j, \ldots\rangle$ of several subsystems.
- `ket[]` is the ket of a system without quantum numbers.

## Details
- Kets and bras describe degrees of freedom that are not represented by fermion operators: impurity spins, phonons, orbital pseudospins, or abstract states. Operators on these degrees of freedom are written as linear combinations of ket-bra products `nc[ket[i], bra[j]]` $= |i\rangle\langle j|$.
- The labels can be numbers or symbols. A label `Null` marks a subsystem that the ket does not refer to: `ket[a, Null]` is a state of the first subsystem only. With `PrettyOutput`, `Null` is displayed as a small circle.
- Rules in `nc` products:

| `nc[bra[i], ket[j]]` | $\langle i \vert j\rangle$ = `braketrule[bra[i], ket[j]]`, by default $\delta_{ij}$ |
| `nc[ket[a, Null], ket[Null, b]]` | `ket[a, b]`: kets of complementary subsystems are combined |
| `nc[ket[a, Null], bra[c, Null], ket[Null, b], bra[Null, d]]` | `nc[ket[a, b], bra[c, d]]` |
| `nc[ket[i], c[CR, ...]]` | `nc[c[CR, ...], ket[i]]`: kets and bras are moved to the right of fermion operators |

- A bra and a ket are evaluated only if they have the same pattern of `Null` entries (the same subsystems). Kets are combined only if their non-`Null` entries are at different positions; otherwise the product stays unevaluated.
- `conj[ket[i]]` is `bra[i]`, and `conj[bra[i]]` is `ket[i]`; `conj` reverses products and conjugates the coefficients.
- In the occupation number representation, a ket is appended to the `vc` vector, `vc[n1, ..., ket[i]]`; see `ap` and `vc`.
- Kets commute with fermion operators. For fermionic degrees of freedom, use fermion operators instead.

## Developer Notes
- The rules are definitions of `nc` and use the private helpers `pairpattern` (same `Null` pattern), `compatiblepattern` (complementary `Null` pattern), and `bkcombine`.
- Macro functions build kets for common systems: `spinket`, `spinbra`, and `spinbasis` for spins, `spinketbraX`, `spinketbraY`, `spinketbraZ`, `spinketbraP`, `spinketbraM` for the spin operators, and `phononbasis`, `phononnumber`, `phononplus`, `phononminus` for a truncated harmonic oscillator.

## Examples
### Basic Examples
```wl
{ket[1], bra[1], ket[x, y]}
```
Scalar products of kets:
```wl
{nc[bra[1], ket[1]], nc[bra[1], ket[2]], nc[bra[x], ket[y]]}
```
A projector acting on a ket:
```wl
nc[ket[0], bra[1], ket[1]]
```

### Scope
Kets of subsystems are combined into product kets:
```wl
nc[ket[1/2, Null], ket[Null, -1/2]]
```
```wl
nc[bra[1/2, -1/2], ket[1/2, -1/2]]
```
---
Kets and bras are moved to the right of fermion operators:
```wl
snegfermionoperators[c];
nc[ket[1], bra[1], c[CR, UP]]
```
---
Conjugation:
```wl
conj[2 I nc[ket[1], bra[2]]]
```
---
Kets in the occupation number representation:
```wl
snegfermionoperators[c];
makebasis[{c[]}];
v = ap[ket[1/2], ap[c[CR, UP], vacuum[]]]
```
```wl
InputForm[v]
```

### Applications
Spin-1/2 operators as ket-bra operators, $S^+ = |\uparrow\rangle\langle\downarrow|$, $S^- = |\downarrow\rangle\langle\uparrow|$, $S^z = (|\uparrow\rangle\langle\uparrow| - |\downarrow\rangle\langle\downarrow|)/2$. They obey $[S^+, S^-] = 2 S^z$:
```wl
sp = nc[ket[1/2], bra[-1/2]];
sm = nc[ket[-1/2], bra[1/2]];
sz = (nc[ket[1/2], bra[1/2]] - nc[ket[-1/2], bra[-1/2]])/2;
Expand[nc[sp, sm] - nc[sm, sp] - 2 sz]
```
---
Truncated harmonic oscillator with at most two quanta. The commutator $[b, b^\dagger]$ is 1 except in the highest state, an artifact of the truncation:
```wl
b = Sum[Sqrt[n] nc[ket[n - 1], bra[n]], {n, 1, 2}];
bd = conj[b];
Expand[nc[b, bd] - nc[bd, b]]
```
---
Kondo coupling $J\, \mathbf S \cdot \mathbf s$ between an impurity spin, represented by kets, and the spin of a conduction electron level $c$. In the space with one electron, the eigenvalues are $-3J/4$ (singlet) and $J/4$ (triplet):
```wl
snegfermionoperators[c];
snegrealconstants[J];
H = J Expand[spinxyz[c[]] ~inner~ spinketbraXYZ[1/2]];
states = Flatten[Outer[nc, {c[CR, UP], c[CR, DO]}, {ket[1/2], ket[-1/2]}]];
Eigenvalues[matrixrepresentationop[H, states]]
```

### Properties & Relations
Kets with the same non-`Null` positions are not combined:
```wl
nc[ket[1], ket[2]]
```
---
`ketbratensorproduct` pads kets and bras with `Null` entries to form tensor products:
```wl
ketbratensorproduct[ket[1], ket[2]]
```

### Possible Issues
A bra and a ket with different patterns of `Null` entries are not evaluated:
```wl
nc[bra[1, Null], ket[1, 2]]
```
