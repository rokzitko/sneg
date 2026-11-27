---
keywords: formatting, pretty printing, output form, daggers, arrows, display
see also: PrettyInput, snegtex, snegfermionoperators, nc
---
# PrettyOutput

## Usage
- `PrettyOutput` is a global setting: if `True` (the default), operators and products are formatted in the conventional notation, e.g. $c^\dagger_{1\uparrow}\cdot c_{2\downarrow}$.

## Details
- With `PrettyOutput = True`, SNEG defines output formats for fermion and boson operators (daggers, spin arrows), spin operators ($S^z_j$), X-operators ($X^{ab}_j$), Grassmann conjugates ($\bar\xi$), `nc` products (infix with center dots), `KroneckerDelta` ($\delta_{ij}$), `UnitStep` ($\theta(x)$), `VACUUM` (|⌀⟩), `dd`, `avg`, and others. The formats affect only the display; `InputForm` and `FullForm` show the expressions as they are.
- The formats of an operator family are defined when it is declared. Setting `PrettyOutput = False` after loading SNEG disables the formats of operators declared afterwards; the formats of `nc` and the other functions are defined when SNEG is loaded.
- To disable all formats, set ``Sneg`PrettyOutput = False`` before loading the package.
- For LaTeX output, use `snegtex`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{PrettyOutput, nc[c[CR, 1, UP], c[AN, 2, DO]]}
```
The expression itself:
```wl
InputForm[nc[c[CR, 1, UP], c[AN, 2, DO]]]
```

### Scope
Operators declared after `PrettyOutput = False` are shown in the plain form:
```wl
PrettyOutput = False;
snegfermionoperators[d];
nc[d[CR, 1, UP], d[AN, 2, DO]]
```
```wl
PrettyOutput = True;
```
