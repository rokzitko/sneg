---
keywords: projector specification, the singly occupied state with spin down
see also: projector, projectorDO, PROJ0, PROJUP, PROJ2, PROJ1, PROJ02
---
# PROJDO

## Usage
- `PROJDO` specifies the projector onto the singly occupied state with spin down in `projector[c[i], PROJDO]`.

## Details
- `PROJDO` is the protected integer constant 3. The constants `PROJ0`, `PROJUP`, `PROJDO`, `PROJ2`, `PROJ1`, `PROJ02` are 1, ..., 6 (the numbering of T. Rejec's operator-chain code).
- `projector[c[i], PROJDO]` is `projectorDO[c[i]]`, $P_\downarrow = n_{i\downarrow}(1 - n_{i\uparrow})$.

## Examples
### Basic Examples
```wl
PROJDO
```
```wl
snegfermionoperators[c];
projector[c[], PROJDO] == projectorDO[c[]]
```
