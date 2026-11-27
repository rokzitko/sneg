---
keywords: no reordering, ordering value, operator order
see also: ordering, EMPTY, SEA, vevwick, contraction
---
# NONE

## Usage
- `NONE` is the value of `ordering[c]` that disables the automatic reordering of products of the operators `c`.

## Details
- With `ordering[c] = NONE`, products are kept in the order in which they are entered; operators of other families are not exchanged with `c` either. The Pauli principle and linearity still apply. It can be set for fermions and bosons. Expectation values of such products are evaluated by `vevwick`. See `ordering`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
ordering[c] = NONE;
{nc[c[AN, 1, UP], c[CR, 1, UP]], vevwick[nc[c[AN, 1, UP], c[CR, 1, UP]]]}
```
