---
keywords: projector specification, the singly occupied state with spin up
see also: projector, projectorUP, PROJ0, PROJDO, PROJ2, PROJ1, PROJ02
---
# PROJUP

## Usage
- `PROJUP` specifies the projector onto the singly occupied state with spin up in `projector[c[i], PROJUP]`.

## Details
- `PROJUP` is the protected integer constant 2. The constants `PROJ0`, `PROJUP`, `PROJDO`, `PROJ2`, `PROJ1`, `PROJ02` are 1, ..., 6 (the numbering of T. Rejec's operator-chain code).
- `projector[c[i], PROJUP]` is `projectorUP[c[i]]`, $P_\uparrow = n_{i\uparrow}(1 - n_{i\downarrow})$.

## Examples
### Basic Examples
```wl
PROJUP
```
```wl
snegfermionoperators[c];
projector[c[], PROJUP] == projectorUP[c[]]
```
