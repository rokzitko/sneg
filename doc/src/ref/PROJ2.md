---
keywords: projector specification, the doubly occupied state
see also: projector, projector2, PROJ0, PROJUP, PROJDO, PROJ1, PROJ02
---
# PROJ2

## Usage
- `PROJ2` specifies the projector onto the doubly occupied state in `projector[c[i], PROJ2]`.

## Details
- `PROJ2` is the protected integer constant 4. The constants `PROJ0`, `PROJUP`, `PROJDO`, `PROJ2`, `PROJ1`, `PROJ02` are 1, ..., 6 (the numbering of T. Rejec's operator-chain code).
- `projector[c[i], PROJ2]` is `projector2[c[i]]`, $P_2 = n_{i\uparrow} n_{i\downarrow}$.

## Examples
### Basic Examples
```wl
PROJ2
```
```wl
snegfermionoperators[c];
projector[c[], PROJ2] == projector2[c[]]
```
