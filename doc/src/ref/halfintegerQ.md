---
keywords: half-integer, spin quantum number, test
see also: snegspinoperators, spinmatrixX, spinbasis, IntegerQ
---
# halfintegerQ

## Usage
- `halfintegerQ[z]` gives `True` if *z* is an integer or a half-integer, and `False` otherwise.

## Details
- `halfintegerQ[z]` is `IntegerQ[2 z]`: it is `True` for exact integers and half-integers, positive or negative, and `False` for symbols, approximate numbers, and other rational numbers.
- It is used to check spin quantum numbers, e.g. in `spinmatrixX` and `spinbasis`.

## Examples
### Basic Examples
```wl
{halfintegerQ[3/2], halfintegerQ[-1/2], halfintegerQ[2], halfintegerQ[1/3]}
```

### Possible Issues
Approximate numbers and symbols give `False`:
```wl
{halfintegerQ[1.5], halfintegerQ[s]}
```
