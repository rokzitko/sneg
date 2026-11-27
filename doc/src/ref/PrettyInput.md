---
keywords: input notation, center dot, CenterDot, infix product
see also: PrettyOutput, nc, CenterDot
---
# PrettyInput

## Usage
- `PrettyInput` is a global setting: if `True` (the default), the center dot `a·b` (`CenterDot`) is interpreted as the operator product `nc[a, b]`.

## Details
- With `PrettyInput = True`, SNEG defines `CenterDot[a, b, ...] := nc[a, b, ...]` when it is loaded, so that output in the format of `PrettyOutput` can be copied and used as input. The center dot is entered as `Esc . Esc`.
- To disable the definition, set ``Sneg`PrettyInput = False`` before loading the package. Changing `PrettyInput` after loading has no effect.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
CenterDot[c[AN, 1, UP], c[CR, 1, UP]]
```
