---
keywords: empty vacuum, normal order, ordering value
see also: ordering, SEA, NONE, snegOrderedQ, VACUUM
---
# EMPTY

## Usage
- `EMPTY` is the value of `ordering[c]` for the normal order of the fermion operators `c` with respect to the empty vacuum.

## Details
- `ordering[c] = EMPTY` is the default for every declared fermion family: the reference state is the state without particles, $c_\alpha|0\rangle = 0$ for all α, and products are normal ordered with creation operators to the left. See `ordering`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{ordering[c], nc[c[AN, 1, UP], c[CR, 1, UP]]}
```
---
Restore the default after another setting:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
ordering[c] = EMPTY;
nc[c[CR, -1, UP], c[AN, -1, UP]]
```
