---
keywords: spin down, spin index, spin projection
see also: UP, CR, AN, snegfermionoperators, number, spinz
---
# DO

## Usage
- `DO` is the spin index of a spin-down fermion: `c[CR, j, DO]` is $c^\dagger_{j\downarrow}$.

## Details
- `DO` has the value 0, and `UP` the value 1; the values are fixed (`DO` is `Protected`). See `UP` for details.
- In the formatted output, `DO` is shown as ↓.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{c[AN, 2, DO], DO, number[c[2], DO]}
```

### Possible Issues
`DO` is the number 0, so it cannot be used as the name of a loop variable or a function:
```wl
{DO == 0, Head[DO]}
```
