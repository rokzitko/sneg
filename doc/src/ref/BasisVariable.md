---
keywords: BASIS, mode order, single-particle basis, occupation vectors, Fock space
see also: makebasis, vc, vc2ops, ops2vc, vacuum, basis
---
# BASIS

## Usage
- `BASIS` is the current ordered list of creation operators used to interpret occupation vectors.

## Details
- `makebasis` expands the levels into individual spin-orbitals. Position *j* in `vc` belongs to entry *j* of `BASIS`. A set bit specifies a creation operator; the order of its predecessors determines the sign on applying an annihilator.
- Basis generators may change this global list. Save it with numerical matrices and with occupation vectors. `basis[c[i]]` instead constructs the many-body states of a single level.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[2], c[1]}];
InputForm[BASIS]
```
### Applications
For a doubly occupied orbital the down annihilator crosses the up creator:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
{ap[d[AN, UP], vc[1,1]], ap[d[AN, DO], vc[1,1]]}
```
