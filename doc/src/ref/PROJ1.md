---
keywords: projector specification, the singly occupied states
see also: projector, projector1, PROJ0, PROJUP, PROJDO, PROJ2, PROJ02
---
# PROJ1

## Usage
- `PROJ1` specifies the projector onto the singly occupied states in `projector[c[i], PROJ1]`.

## Details
- `PROJ1` is the protected integer constant 5. The constants `PROJ0`, `PROJUP`, `PROJDO`, `PROJ2`, `PROJ1`, `PROJ02` are 1, ..., 6 (the numbering of T. Rejec's operator-chain code).
- `projector[c[i], PROJ1]` is `projector1[c[i]]`, $P_1 = P_\uparrow + P_\downarrow = n_i - 2 n_{i\uparrow} n_{i\downarrow}$.

## Examples
### Basic Examples
```wl
PROJ1
```
```wl
snegfermionoperators[c];
projector[c[], PROJ1] == projector1[c[]]
```
