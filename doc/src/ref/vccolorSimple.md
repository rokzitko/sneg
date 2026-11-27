---
keywords: output formatting, display of occupation number states
see also: vccolor, vccolorDefault, vc
---
# vccolorSimple

## Usage
- `vccolorSimple[{n1, n2, ...}]` gives display elements of the occupation numbers of a `vc` vector without spin coloring: filled rectangles for occupied and open rectangles for empty states.

## Details
- Set `vccolor = vccolorSimple` to use this display for all `vc` vectors; `vccolor = vccolorDefault` restores the default.

## Examples
### Basic Examples
```wl
vccolorSimple[{1, 0, 0, 1}]
```
Display of the states of three spinless fermion levels:
```wl
snegspinlessfermionoperators[a];
vccolor = vccolorSimple;
qbasisvc[{a[1], a[2], a[3]}]
```
```wl
vccolor = vccolorDefault;
```
