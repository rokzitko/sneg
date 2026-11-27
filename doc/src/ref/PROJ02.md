---
keywords: projector specification, the empty and the doubly occupied states
see also: projector, projector02, PROJ0, PROJUP, PROJDO, PROJ2, PROJ1
---
# PROJ02

## Usage
- `PROJ02` specifies the projector onto the empty and the doubly occupied states in `projector[c[i], PROJ02]`.

## Details
- `PROJ02` is the protected integer constant 6. The constants `PROJ0`, `PROJUP`, `PROJDO`, `PROJ2`, `PROJ1`, `PROJ02` are 1, ..., 6 (the numbering of T. Rejec's operator-chain code).
- `projector[c[i], PROJ02]` is `projector02[c[i]]`, $P_{02} = P_0 + P_2 = (n_i - 1)^2$.

## Examples
### Basic Examples
```wl
PROJ02
```
```wl
snegfermionoperators[c];
projector[c[], PROJ02] == projector02[c[]]
```
