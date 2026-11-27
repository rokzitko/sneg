---
keywords: inner product, scalar product, dot product, noncommutative product, spin-spin interaction, S.S
see also: outer, VMV, mcross, nc, spinxyz, spinspin, Inner
---
# inner

## Usage
- `inner[a, b]` gives the inner product $\sum_i a_i b_i$ of the lists *a* and *b*, with the products formed by `nc`.
- `inner[a, b, g]` uses the function *g* instead of `Plus` to combine the products.

## Details
- `inner[a, b, ...]` is `Inner[nc, a, b, ...]`. The order of the factors in each product is preserved, as needed for operators; for operator vectors $\mathbf A$ and $\mathbf B$, `inner[A, B]` is $\mathbf A \cdot \mathbf B = \sum_a A^a B^a$.
- As for `Inner`, *a* and *b* can be tensors: the last index of *a* is contracted with the first index of *b*, e.g. for a matrix and a vector.
- No complex conjugation is performed. For the scalar product of states, use `scalarproductvc` or `scalarproductop`.
- The result is not expanded.

## Examples
### Basic Examples
```wl
inner[{a1, a2}, {b1, b2}]
```
Spin-spin interaction $\mathbf S_1 \cdot \mathbf S_2$ of two fermion levels:
```wl
snegfermionoperators[c];
inner[spinxyz[c[1]], spinxyz[c[2]]] // Expand
```

### Scope
Product of a matrix and a vector of operators:
```wl
snegfermionoperators[c];
inner[{{0, 1}, {1, 0}}, {c[AN, 1, UP], c[AN, 2, UP]}]
```

### Applications
Square of the spin of a single level, $\mathbf S^2 = \frac34 (n - 2 n_\uparrow n_\downarrow)$, which is nonzero only for single occupancy:
```wl
snegfermionoperators[d];
s = spinxyz[d[]];
Expand[inner[s, s] - 3/4 (number[d[]] - 2 hubbard[d[]])]
```
---
For a spin operator with $S = 1$, whose components are `S[1]`, `S[2]`, `S[3]`, $\mathbf S^2 = S(S+1) = 2$:
```wl
snegspinoperators[{S, 1}];
inner[{S[1], S[2], S[3]}, {S[1], S[2], S[3]}]
```

### Properties & Relations
`inner[s1, s2]` equals `spinspin`:
```wl
snegfermionoperators[c];
Expand[inner[spinxyz[c[1]], spinxyz[c[2]]] - spinspin[c[1], c[2]]]
```
---
The order of the factors matters for operators:
```wl
snegfermionoperators[c];
{inner[{c[CR, 1, UP]}, {c[AN, 1, UP]}], inner[{c[AN, 1, UP]}, {c[CR, 1, UP]}]}
```
