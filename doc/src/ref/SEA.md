---
keywords: Fermi sea, particle-hole, normal order, ordering value, Fermi level
see also: ordering, EMPTY, NONE, iscreation, isannihilation, vev, VACUUM
---
# SEA

## Usage
- `SEA` is the value of `ordering[c]` for the normal order of the fermion operators `c` with respect to the Fermi sea.

## Details
- With `ordering[c] = SEA`, the second index of `c[t, k, ...]` is the momentum or energy measured from the Fermi level: the levels with $k < 0$ are filled and those with $k > 0$ empty in the reference state $|\mathrm{FS}\rangle$. Then $c^\dagger_k$ ($k > 0$) and $c_k$ ($k < 0$) are quasiparticle creation operators, and products are normal ordered with respect to $|\mathrm{FS}\rangle$. See `ordering` for details and examples.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{nc[d[CR, -1, UP], d[AN, -1, UP]], vev[nc[d[CR, -1, UP], d[AN, -1, UP]]], vev[nc[d[CR, 1, UP], d[AN, 1, UP]]]}
```
