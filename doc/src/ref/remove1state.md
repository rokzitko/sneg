---
keywords: projection, orthogonal complement, remove state, orthonormal basis
see also: removestates, orthogvc, scalarproductvc, transformQStoIS
---
# remove1state

## Usage
- `remove1state[vecs, s]` gives an orthonormal basis of the orthogonal complement of the state *s* in the space spanned by the orthonormal states *vecs*; all states are in the occupation number representation.

## Details
- Each state $v$ of *vecs* is replaced by $v - \langle s | v\rangle\, s$, and the results are orthonormalized with `orthogvc[..., vecs]`. The result has one state fewer than *vecs* if *s* lies in the span of *vecs*.
- *vecs* and *s* must be normalized; *vecs* must be orthonormal.
- `remove1state[{}, s]` is `{}`. `removestates` removes several states.
- `transformQStoIS` uses `removestates` to project the isospin multiplet partners out of the subspaces with lower charge.

## Examples
### Basic Examples
Removing the triplet state $(|\uparrow\downarrow\rangle + |\downarrow\uparrow\rangle)/\sqrt 2$ from the two-site states with $S_z = 0$ and single occupancy leaves the singlet:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
remove1state[{vc[1, 0, 0, 1], vc[0, 1, 1, 0]}, (vc[1, 0, 0, 1] + vc[0, 1, 1, 0])/Sqrt[2]] // InputForm
```
