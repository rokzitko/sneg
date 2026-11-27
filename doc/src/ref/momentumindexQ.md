---
keywords: momentum index test, predicate
see also: latticeindexQ, latticeindextype, snegmomentumindexes, latticesize
---
# momentumindexQ

## Usage
- `momentumindexQ[k]` gives `True` if *k* is a symbol declared as a momentum index with `snegmomentumindexes`, and `False` otherwise.

## Details
- `momentumindexQ[k]` is `latticeindextype[k] === "Momentum"`. Expressions such as `k + q` give `False`.

## Examples
### Basic Examples
```wl
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k, q}, Ns];
{momentumindexQ[k], momentumindexQ[R], momentumindexQ[k + q]}
```
