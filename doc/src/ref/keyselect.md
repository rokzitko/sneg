---
keywords: select subspace, quantum numbers, key, basis, invariant subspace
see also: keymerge, mergebasis, makematricesbzvc, qszbasisvc, Select, Cases
---
# keyselect

## Usage
- `keyselect[list, key]` gives the elements of *list* whose first part equals *key*.

## Details
- `keyselect[list, key]` is `Select[list, First[#] == key &]`. Applied to a basis or to a list of matrices `{{qn, ...}, ...}`, it gives the list of elements for the subspace with quantum numbers *key*, i.e. `{{key, states}}`; use `keyselect[bz, key][[1, 2]]` to extract the states or the matrix.
- The comparison uses `Equal`. Patterns are not supported as keys; use `Cases[list, {{0, _}, _}]` to select by pattern.

## Examples
### Basic Examples
The half-filled singlet subspace of the two-site $(Q, S)$ basis:
```wl
snegfermionoperators[c];
bz = qsbasisvc[{c[1], c[2]}];
keyselect[bz, {0, 0}]
```
The Hamiltonian matrix of the Hubbard dimer in this subspace:
```wl
snegrealconstants[t, U];
keyselect[makematricesbzvc[-t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]], bz], {0, 0}][[1, 2]]
```

### Possible Issues
A key that does not occur gives an empty list, so that extracting the states fails. With two levels there is no doublet at half filling:
```wl
snegfermionoperators[c];
keyselect[qsbasisvc[{c[1], c[2]}], {0, 1/2}][[1, 2]]
```
