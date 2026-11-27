---
keywords: projection, orthonormalization, subspace, occupation number representation
see also: transformbasis, orthogvc, projector1, ap
---
# transformfunc

## Usage
- `transformfunc[states, op]` applies the operator *op* to the list of *states* in the occupation number representation and gives an orthonormal basis of the span of the results within the span of *states*.

## Details
- `transformfunc` is the step of `transformbasis` for a single subspace. The states must be orthonormal. The vectors $\hat O |i\rangle$ are expanded in the *states* (components outside their span are discarded) and orthonormalized with `orthogvc`; vectors that vanish are dropped.
- The number of returned states is the rank of $\hat O$ on the span of *states*.

## Examples
### Basic Examples
Projection of the two-electron states with $S_z = 0$ of two levels onto single occupancy of both levels:
```wl
snegfermionoperators[c];
states = keyselect[qszbasisvc[{c[1], c[2]}], {0, 0}][[1, 2]]
```
```wl
transformfunc[states, nc[projector1[c[1]], projector1[c[2]]]]
```

### Properties & Relations
For a non-projector, the results are orthonormalized; here $1 + c^\dagger_{2\uparrow} c_{1\uparrow}$ acting on the one-electron states with spin up:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
transformfunc[{vc[1, 0, 0, 0], vc[0, 0, 1, 0]}, 1 + nc[c[CR, 2, UP], c[AN, 1, UP]]]
```
