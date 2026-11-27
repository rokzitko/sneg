---
keywords: finite ring, periodic cluster, momentum sums, momentum conservation, lattice vectors
see also: fourier, fourierrules, clusterhop, momentumbasis, sum, blochphase
---
# sumCluster

## Usage
- `sumCluster[expr, dims]` evaluates typed lattice and momentum sums on a finite periodic cluster with side lengths *dims*.
- `sumCluster[expr, dims, rules]` substitutes the lattice vectors specified in *rules*.

## Details
- Sites and momenta have integer labels starting at zero. Indices are folded modulo the cluster dimensions before evaluating operator products and momentum deltas. In dimension greater than one supply vectors in lattice coordinates.
- Symbolic lattice sizes become the product of the side lengths. Do not use a conflicting declared numeric size. Untyped dummy indices remain symbolic.
- Only periodic boundary conditions are supported. For a twist, build the bonds with `clusterhop` and transform with `fourierrules`.

| `"IndexPosition"` | `1` | position of the lattice label after the operator type |
| `"Operators"` | `Automatic` | operator heads whose labels are folded, inferred by default |

## Examples
### Basic Examples
The term crossing the boundary of a three-site ring is included once:
```wl
snegspinlessfermionoperators[f]; sneglatticeindexes[{R}, Ns];
ring = sumCluster[sum[hop[f[R], f[R+1]], {R}], {3}];
Expand[ring - clusterhop[1, f, f, 1, {3}]]
```
### Applications
Periodicity identifies the neighbours in the density at sites $R$ and $R+L$:
```wl
snegfermionoperators[c]; sneglatticeindexes[{R}, Ns];
sumCluster[sum[number[c[R+3]] - number[c[R]], {R}], {3}]
```
