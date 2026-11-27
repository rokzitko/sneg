---
keywords: spin size, spin of fermion, number of flavors, spin index
see also: snegfermionoperators, snegspinlessfermionoperators, snegspinoperators, number, hubbard, basis
---
# spinof

## Usage
- `spinof[c]` gives the spin $s$ of the fermion operator family *c* or the spin size of the spin operators *c*.

## Details
- For fermions declared with `snegfermionoperators[c]`, `spinof[c]` is 1/2; with `snegfermionoperators[{c, s}]` it is *s*, and with `snegspinlessfermionoperators[c]` it is 0. The spin index then takes the $2s + 1$ values $-s, \ldots, s$ (`DO`, `UP` for $s = 1/2$).
- For spin operators declared with `snegspinoperators[{S, s}]`, it is *s*.
- Functions such as `number`, `hubbard`, `basis`, and the basis constructors use `spinof` to generate the spin indexes.
- `spinof[x]` is 1/2 for every *x* without another value, including undeclared symbols and spin operators declared without size.
- The value is an upvalue and can be changed after the declaration with `spinof[c] ^= s`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, {f, 3/2}];
snegspinlessfermionoperators[g];
{spinof[c], spinof[f], spinof[g]}
```
The number operator sums over the spin projections:
```wl
number[f[]]
```

### Possible Issues
The default value 1/2 is returned also for symbols that are not operators:
```wl
spinof[u]
```
