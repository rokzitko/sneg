---
keywords: projector specification, the empty state
see also: projector, projector0, PROJUP, PROJDO, PROJ2, PROJ1, PROJ02
---
# PROJ0

## Usage
- `PROJ0` specifies the projector onto the empty state in `projector[c[i], PROJ0]`.

## Details
- `PROJ0` is the protected integer constant 1. The constants `PROJ0`, `PROJUP`, `PROJDO`, `PROJ2`, `PROJ1`, `PROJ02` are 1, ..., 6 (the numbering of T. Rejec's operator-chain code).
- `projector[c[i], PROJ0]` is `projector0[c[i]]`, $P_0 = (1 - n_{i\uparrow})(1 - n_{i\downarrow})$.

## Examples
### Basic Examples
```wl
PROJ0
```
```wl
snegfermionoperators[c];
projector[c[], PROJ0] == projector0[c[]]
```
