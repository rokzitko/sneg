---
keywords: crystal momentum, many-body momentum, Bloch basis, symmetry sectors, exact diagonalization
see also: qszbasis, qszbasisvc, fourierrules, sumCluster, makematricesbzvc
---
# momentumbasis

## Usage
- `momentumbasis[bz, dims]` splits each sector of a momentum-orbital basis by its total crystal momentum and appends the momentum label to the quantum numbers.

## Details
- For creation operators with momentum labels $m_j$ the total label is $\sum_j m_j$ modulo the side length along each direction; the empty state has zero total momentum. These are grid labels, not angles.
- Input states must already have definite momentum. This function groups them; it does not Fourier transform real-space states. A linear combination of states with different momenta gives a message and `$Failed`.
- For occupation vectors, retain the single-particle `BASIS` belonging to the input.

| `"IndexPosition"` | `1` | position of the momentum label after the operator type |

## Examples
### Basic Examples
On a two-site ring with one electron of each spin the pairs with both electrons at the same momentum have $K=0$. A pair with one electron at each momentum has $K=1$:
```wl
snegfermionoperators[f];
b = momentumbasis[qszbasis[{f[0],f[1]}],{2}];
TableForm[{Last[First[#]],Length[Last[#]]} & /@ Select[b,Take[First[#],2] == {0,0} &],
 TableHeadings -> {None,{"K","States"}}]
```
