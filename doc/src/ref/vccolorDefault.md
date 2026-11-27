---
keywords: output formatting, display of occupation number states, spin-1/2
see also: vccolor, vccolorSimple, vc
---
# vccolorDefault

## Usage
- `vccolorDefault[{n1, n2, ...}]` gives the default display elements of the occupation numbers of a `vc` vector: filled (occupied) and open (empty) rectangles, red for odd positions (spin up) and blue for even positions (spin down).

## Details
- `vccolorDefault` is the default value of `vccolor`. It assumes that the entries come in pairs $(n_{i\uparrow}, n_{i\downarrow})$, as for spin-1/2 levels; for an odd number of entries, the last entry is taken to be a ket and is shown unchanged.
- For spinless fermions or fermions with other spins, the coloring does not correspond to the spin; use `vccolorSimple`.

## Examples
### Basic Examples
```wl
vccolorDefault[{1, 0, 0, 1}]
```
States with a ket:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
vc[1, 0, ket[1/2]]
```
