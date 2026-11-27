---
keywords: spin operator test, type predicate, declared operator
see also: operatorQ, snegspinoperators, spinof, xoperatorQ
---
# spinQ

## Usage
- `spinQ[S]` gives `True` if *S* is a spin operator symbol or a spin operator `S[...]`.

## Details
- `spinQ` is `True` for symbols declared with `snegspinoperators` and for their applications `S[j, ..., t]`.
- For other expressions, `spinQ` remains unevaluated; use `TrueQ[spinQ[x]]` in conditions.
- The spin operators of fermion levels (`spinz[c[]]`, ...) are operator expressions, not spin operators in this sense.
- The spin size is `spinof[S]`.

## Examples
### Basic Examples
```wl
snegspinoperators[{S, 1/2}];
{spinQ[S], spinQ[S[1, 3]], spinof[S]}
```

### Possible Issues
`spinQ` is not defined for the spin operators of fermion levels:
```wl
snegfermionoperators[c];
TrueQ[spinQ[spinz[c[]]]]
```
