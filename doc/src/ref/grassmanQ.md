---
keywords: Grassmann test, type predicate, anticommuting number
see also: sneggrassmanconstants, int, isnumericQ, operatorQ
---
# grassmanQ

## Usage
- `grassmanQ[x]` gives `True` if *x* is a Grassmann number declared with `sneggrassmanconstants`, or the conjugate `conj[x]` of one.

## Details
- `grassmanQ` is `True` for the declared symbols and their conjugates. For other expressions, including products of Grassmann numbers, it remains unevaluated; use `TrueQ[grassmanQ[x]]` in conditions.
- Grassmann numbers are neither operators (`operatorQ`) nor scalars (`isnumericQ`).

## Examples
### Basic Examples
```wl
sneggrassmanconstants[x];
{grassmanQ[x], grassmanQ[conj[x]], TrueQ[grassmanQ[y]], isnumericQ[x]}
```
