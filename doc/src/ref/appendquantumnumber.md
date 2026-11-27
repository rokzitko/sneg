---
keywords: basis, quantum numbers, parity, additional quantum number
see also: mergebasis, applybasis, transformtoLR, mapbasis
---
# appendquantumnumber

## Usage
- `appendquantumnumber[bz, qn]` appends the quantum number *qn* to the quantum numbers of every subspace of the basis *bz*.

## Details
- The result is `{{Append[qns, qn], states}, ...}`; the states are not changed.
- `transformtoLR` uses `appendquantumnumber` to label the even and odd subspaces with the mirror parity $P = 1$ and $P = -1$ before joining them.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
appendquantumnumber[qszbasis[{d[]}], 1]
```

### Applications
Labeling the states of a single level with the parity $P = 1$, so that the basis can be combined with bases with three quantum numbers $(Q, S, P)$:
```wl
snegfermionoperators[d];
appendquantumnumber[qsbasis[{d[]}], 1]
```
