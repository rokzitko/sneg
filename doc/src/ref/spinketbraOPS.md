---
keywords: bra-ket representation, spin operators, ladder operators
see also: spinketbraX, spinketbraXYZ, spinketbrarules
---
# spinketbraOPS

## Usage
- `spinketbraOPS[s]` gives the list $\{S^x, S^y, S^z, S^+, S^-\}$ of the spin-*s* operators in the bra-ket representation.
- `spinketbraOPS[s, pos]` places the spin in the slots specified by the position array *pos*.

## Details
- The order corresponds to the component indexes 1, ..., 5 of abstract spin operators `S[j, t]` (see `snegspinoperators`), so that `Thread[Table[S[t], {t, 5}] -> spinketbraOPS[s]]` maps a single spin to the bra-ket representation; `spinketbrarules` generalizes this to several spins.
- See `spinketbraX` for the position arrays.

## Examples
### Basic Examples
```wl
spinketbraOPS[1/2]
```

### Applications
Map an abstract spin-1/2 operator without a site index to the bra-ket representation:
```wl
snegspinoperators[{S, 1/2}];
Expand[nc[S[4], S[5]] /. Thread[Table[S[t], {t, 5}] -> spinketbraOPS[1/2]]]
```
