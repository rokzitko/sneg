---
keywords: FullSimplify, summand, symbolic sum
see also: ruleExpandUnderSum, rulesumDistribute, sumFullSimplify, sum, FullSimplify
---
# ruleFullSimplifyUnderSum

## Usage
- `expr /. ruleFullSimplifyUnderSum` applies `FullSimplify` to the summands of the symbolic sums in *expr*.

## Details
- `ruleFullSimplifyUnderSum` is `{sum[a_, {q__}] :> sum[FullSimplify[a], {q}]}`. Only the summands are simplified; the sums themselves are not expanded, collected, or combined, in contrast to `sumFullSimplify`.
- With `/.`, only the outermost sums are matched; use `//.` for nested sums.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegrealfunctions[eps];
s = sum[Sin[eps[k]]^2 c[CR, k, UP] + Cos[eps[k]]^2 c[CR, k, UP], {k}]
```
```wl
s /. ruleFullSimplifyUnderSum
```
