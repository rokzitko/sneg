---
keywords: equations of motion, closure, commutator matrix, Liouvillian, interacting Green's function
see also: greensfunction, liouvillian, spectralmoments, eigenoperators, komutator
---
# eomclosure

## Usage
- `eomclosure[A, H]` returns `{ops, M, a}` such that `A == a.ops` and the commutator of `ops[[i]]` with *H* is `M[[i]].ops`.

## Details
- `ops` is a list of the distinct normal-ordered operator monomials generated from *A* by commutation with *H*. For $r$ monomials, $M$ is an $r\times r$ matrix and $a$ has length $r$. The commutator is $[O_i,H]$, not $[H,O_i]$.
- Rows give the images of the basis operators. Consequently the resolvent in `greensfunction` is contracted with $a$ from the left. The ordering of monomials is not a physical quantum number.
- Both arguments must be free of symbolic `sum`; *A* must have definite fermion parity. `eomclosure::sum`, `eomclosure::parity` and `eomclosure::term` indicate an unsupported input.

| `"MaxOperators"` | `100` | stop with `eomclosure::open` and `$Failed` if a further distinct monomial is required; reaching the limit is not a physical decoupling approximation |

## Examples
### Basic Examples
For a Hubbard atom the interaction generates one additional operator, the annihilator conditioned on occupation by the opposite spin:
```wl
snegfermionoperators[d];
snegrealconstants[e, U];
h = e number[d[]] + U hubbard[d[]];
{ops, m, a} = eomclosure[d[AN, UP], h]
```
Verify the row convention and the coefficients of the starting operator:
```wl
{Expand[a.ops - d[AN, UP]], Expand[(komutator[#, h] & /@ ops) - m.ops]}
```

### Applications
For a noninteracting chain, successive commutators propagate the electron to neighbouring sites. No creation or composite operators are generated:
```wl
snegfermionoperators[c];
snegrealconstants[t];
h = -t (hop[c[1], c[2]] + hop[c[2], c[3]]);
{ops, m, a} = eomclosure[c[AN, 1, UP], h];
{ops, Factor[CharacteristicPolynomial[m, z]]}
```

### Possible Issues
Even a single interacting site needs more than one monomial. A one-operator limit returns no approximate matrix:
```wl
snegfermionoperators[d];
eomclosure[d[AN, UP], hubbard[d[]], "MaxOperators" -> 1]
```
