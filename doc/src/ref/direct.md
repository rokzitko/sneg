---
keywords: direct product, product of states, operator strings, basis construction
see also: basis, directproductbasis, basistensorproduct, outer
---
# direct

## Usage
- `direct[l1, l2]` gives the flattened list of all products `nc[a, b]` with *a* from the list *l1* and *b* from *l2*.

## Details
- `direct[l1, l2]` is `Flatten[outer[l1, l2]]`, where `outer` is `Outer` with `nc` as the product. The elements of *l1* stand to the left.
- `direct` is used to build bases of several levels from single-level bases (`basis`) in the creation operator representation.

## Examples
### Basic Examples
```wl
direct[{2, 3}, {a, b}]
```

### Applications
The 16 states of two levels:
```wl
snegfermionoperators[c];
b2 = direct[basis[c[1]], basis[c[2]]]
```
They are orthonormal:
```wl
Outer[scalarproductop, b2, b2] == IdentityMatrix[16]
```
---
Partition function of the Hubbard dimer $H = -t \sum_\sigma (c^\dagger_{1\sigma} c_{2\sigma} + \mathrm{h.c.}) + U \sum_i n_{i\uparrow} n_{i\downarrow}$ to second order in $\beta$, $Z = \mathrm{Tr}\, 1 - \beta\, \mathrm{Tr}\, H + \frac{\beta^2}{2} \mathrm{Tr}\, H^2$:
```wl
snegrealconstants[t, U, beta];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
h = matrixrepresentationop[H, b2];
Expand[Tr[IdentityMatrix[16]] - beta Tr[h] + beta^2/2 Tr[h . h]]
```
