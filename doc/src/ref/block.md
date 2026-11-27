---
keywords: expansion, grouping, operator sums, inert wrapper
see also: nc, sumExpand, SnegSimplify
---
# block

## Usage
- `block[expr]` wraps an expression to shield its top-level sum from automatic distribution; `nc[block[expr]]` gives *expr*.

## Details
- The argument evaluates as usual before wrapping; this is not a holding construct like `HoldForm`. The lowercase `block` has no relation to `Block`, which localizes Wolfram Language values.
- The wrapper does not define the statistics of a composite operator. Remove it before evaluating an explicit commutator, matrix element or vacuum average.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
wrapped = block[c[CR,UP] + c[AN,UP]];
nc[wrapped]
```
