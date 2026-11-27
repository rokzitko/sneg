---
keywords: spin up, spin index, spin projection
see also: DO, CR, AN, snegfermionoperators, number, spinz
---
# UP

## Usage
- `UP` is the spin index of a spin-up fermion: `c[CR, j, UP]` is $c^\dagger_{j\uparrow}$.

## Details
- `UP` has the value 1, and `DO` the value 0; the values are fixed (`UP` is `Protected`). The spin index is the last index of a fermion operator.
- For fermions with spin $s \ne 1/2$ (`snegfermionoperators[{c, s}]`), the spin index takes the values $-s, \ldots, s$; `UP` and `DO` are then the numbers 1 and 0 and are interpreted as such.
- In the formatted output, the spin index of spin-1/2 operators is shown as ↑ and ↓.
- Since `DO` < `UP`, operators of the same type and orbital are ordered with spin down first.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{c[CR, 1, UP], UP, InputForm[c[CR, 1, UP]]}
```
Functions that act on a level take the spin index as an optional argument:
```wl
number[c[1], UP]
```

### Properties & Relations
Spin-down operators are ordered before spin-up operators:
```wl
snegfermionoperators[c];
nc[c[CR, 1, UP], c[CR, 1, DO]]
```
---
For spin 1, `UP` = 1 is the projection $m = 1$ and `DO` = 0 the projection $m = 0$:
```wl
snegfermionoperators[{f, 1}];
{number[f[]], f[CR, UP] === f[CR, 1]}
```
