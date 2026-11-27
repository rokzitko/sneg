---
keywords: scalar marker, simplify coefficients
see also: rulestripscalar, ruleFullSimplifyUnderSum, Simplify
---
# rulesimplifyscalar

## Usage
- `expr /. rulesimplifyscalar` applies `Simplify` to the contents of all scalar markers `scalar[x]` in *expr*.

## Details
- `rulesimplifyscalar` is `{scalar[x_] :> scalar[Simplify[x]]}`. The marker is kept; use `rulestripscalar` to remove it. See `rulestripscalar` for the meaning of `scalar`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegrealconstants[t];
e = scalar[Sin[t]^2 + Cos[t]^2] c[CR, 1, UP]
```
```wl
e /. rulesimplifyscalar
```
```wl
e /. rulesimplifyscalar /. rulestripscalar
```
