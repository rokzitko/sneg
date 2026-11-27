---
keywords: annihilation operator, quasiparticle, normal order, Fermi sea, particle-hole
see also: iscreation, ordering, snegOrderedQ, AN, vev, VACUUM, zeroonvac
---
# isannihilation

## Usage
- `isannihilation[op]` gives `True` if the fermion or boson operator *op* annihilates the reference state of its family, i.e. if it is an annihilation operator in the sense of the normal order.

## Details
- The reference state is set by `ordering`:

| `ordering[c]` | `isannihilation[c[t, k, ...]]` |
|---|---|
| EMPTY (default) | `t == AN` |
| SEA | `t == AN` and $k > 0$, or `t == CR` and $k < 0$ |
| NONE | `t == AN` |
| bosons | `t == AN` |

- An operator with `isannihilation` `True` acting on `VACUUM` gives zero, and so does `vev` of a product ending with it.
- See `iscreation` for the details and the use as an extension point.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{isannihilation[c[AN, 1, UP]], isannihilation[c[CR, 1, UP]]}
```
For the Fermi sea, $c^\dagger_k$ with $k < 0$ annihilates the Fermi sea (it fills a hole):
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{isannihilation[d[AN, 2, UP]], isannihilation[d[CR, -2, UP]], nc[d[CR, -2, UP], VACUUM]}
```

### Applications
`zeroonvac` drops the terms that annihilate the reference state:
```wl
snegfermionoperators[c];
zeroonvac[nc[c[CR, 1, UP], c[AN, 2, UP]] + c[CR, 3, UP]]
```
