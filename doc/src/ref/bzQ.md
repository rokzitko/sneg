---
keywords: basis, invariant subspaces, test, pattern
see also: makematricesbzvc, makematricesbzop, showbzmat, dropemptysubspaces, qszbasisvc
---
# bzQ

## Usage
- `bzQ[bz]` gives `True` if *bz* has the form of a basis, a nonempty list of pairs `{{qn...}, {states...}}`, and `False` otherwise.

## Details
- A basis is a list `{{qn1, states1}, {qn2, states2}, ...}` of invariant subspaces, where the quantum numbers `qn` and the states are lists; see `makematricesbzvc`.
- `bzQ` checks only the structure, not the states. Lists of matrices `{{qn, mat}, ...}` as returned by `makematricesbzvc` also have this form.
- The empty list `{}` gives `False`.
- `makematricesbzvc`, `makematricesbzop`, `showbzmat`, and `applybasis` require arguments that satisfy `bzQ`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
bzQ[qszbasisvc[{c[1]}]]
```
```wl
{bzQ[{{{0}, {a, b}}}], bzQ[{{0, {a}}}], bzQ[{}]}
```

### Possible Issues
`makematricesbzvc` stays unevaluated if the basis does not satisfy `bzQ`. A single subspace must be wrapped in a list:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1]}];
makematricesbzvc[number[c[1]], bz[[2]]]
```
```wl
makematricesbzvc[number[c[1]], {bz[[2]]}]
```
