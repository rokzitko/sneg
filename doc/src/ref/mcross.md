---
keywords: cross product, vector product, noncommutative, spin algebra, angular momentum commutation relations
see also: inner, outer, VMV, nc, spinxyz, Cross
---
# mcross

## Usage
- `mcross[a, b]` gives the cross product $\mathbf a \times \mathbf b$ of three-component lists *a* and *b*, with the products formed by `nc`.

## Details
- The components are $(\mathbf a \times \mathbf b)_x = a_y b_z - a_z b_y$, and cyclic. The order of the factors is preserved, so $\mathbf a \times \mathbf b \ne -\mathbf b \times \mathbf a$ for noncommuting components.
- Only the first three elements of *a* and *b* are used.
- The su(2) commutation relations $[S^a, S^b] = i \epsilon_{abc} S^c$ are equivalent to $\mathbf S \times \mathbf S = i \mathbf S$.

## Examples
### Basic Examples
```wl
mcross[{a1, a2, a3}, {b1, b2, b3}]
```

### Applications
The spin operators satisfy $\mathbf S \times \mathbf S = i \mathbf S$:
```wl
snegspinoperators[{S, 1/2}];
s = {S[1], S[2], S[3]};
Expand[mcross[s, s] - I s]
```
The same holds for the spin of a fermion level:
```wl
snegfermionoperators[c];
sc = spinxyz[c[]];
Expand[mcross[sc, sc] - I sc]
```

### Properties & Relations
For numbers, `mcross` agrees with `Cross`:
```wl
mcross[{1, 2, 3}, {4, 5, 6}] == Cross[{1, 2, 3}, {4, 5, 6}]
```
---
For operators, the order matters: $\mathbf S \times \mathbf S \ne 0$:
```wl
snegspinoperators[{S, 1/2}];
mcross[{S[1], S[2], S[3]}, {S[1], S[2], S[3]}]
```
