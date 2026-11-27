---
keywords: projection, orthogonal complement, remove states, orthonormal basis
see also: remove1state, orthogvc, transformQStoIS
---
# removestates

## Usage
- `removestates[vecs, states]` gives an orthonormal basis of the orthogonal complement of the normalized states *states* in the space spanned by the orthonormal states *vecs*; all states are in the occupation number representation.

## Details
- `removestates` applies `remove1state` successively for each state of *states*: `Fold[remove1state, vecs, states]`. For the result to be the orthogonal complement, the states in *states* must be mutually orthogonal.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
makebasis[{d[]}];
removestates[{vc[0, 0], vc[1, 0], vc[0, 1]}, {(vc[1, 0] + vc[0, 1])/Sqrt[2], vc[0, 0]}] // InputForm
```

### Applications
The multiplet partner $S^- |\uparrow\uparrow\rangle$ of the triplet removed from the $S_z = 0$, half-filled subspace of two sites leaves the singlet and the two states with a doubly occupied site:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
states = Last[First[Select[bz, First[#] == {0, 0} &]]];
v = ap[spinminus[c[1]] + spinminus[c[2]], vc[1, 0, 1, 0]];
removestates[states, {v/normvc[v]}] // InputForm
```
