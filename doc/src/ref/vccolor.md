---
keywords: output formatting, display of occupation number states, PrettyOutput, vc
see also: vccolorDefault, vccolorSimple, vc, PrettyOutput
---
# vccolor

## Usage
- `vccolor` is the function used to display the occupation numbers of `vc` vectors in `StandardForm`.

## Details
- With `PrettyOutput` (the default), `vc[n1, n2, ...]` is displayed as $\Vert \ldots \rangle$ with the elements returned by `vccolor[{n1, n2, ...}]` in between.
- The default is `vccolorDefault`, which shows occupied states as filled and empty states as open rectangles, alternately red (spin up) and blue (spin down), as appropriate for spin-1/2 levels. `vccolorSimple` shows black rectangles without spin coloring, which is preferable for spinless fermions and fermions with other spins.
- Set `vccolor = vccolorSimple` to change the display, and `vccolor = vccolorDefault` to restore it. Any function that maps the list of entries to a list of displayable elements can be used.
- The formatting does not affect `InputForm` or the internal representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
v = vc[1, 0, 1, 1]
```
```wl
vccolor = vccolorSimple;
v
```
```wl
vccolor = vccolorDefault;
v
```

### Scope
A custom display that shows the occupation numbers as digits:
```wl
vccolor = Function[l, Riffle[ToString /@ l, ","]];
vc[1, 0, 1, 1]
```
```wl
vccolor = vccolorDefault;
```
