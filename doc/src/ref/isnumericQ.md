---
keywords: scalar, c-number, numeric quantity, constant, parameter, linearity, factoring out constants
see also: snegrealconstants, snegcomplexconstants, snegintegerconstants, snegrealfunctions, snegcomplexfunctions, snegfreeindexes, sneglinearoperator, nc, scalar, NumericQ
---
# isnumericQ

## Usage
- `isnumericQ[x]` gives `True` if *x* is a scalar (c-number) that `nc` and the other linear functions of SNEG factor out of operator products, and `False` otherwise.

## Details
- `isnumericQ[x]` is `True` if every subexpression of *x* (every element of `Level[x, {0, Infinity}]`) is
  - a numeric quantity in the sense of `NumericQ`, e.g. `2`, `I`, `Pi`, `Sqrt[2]`;
  - a symbol declared with `snegrealconstants`, `snegcomplexconstants`, `snegintegerconstants`, or `snegfreeindexes`, or an application `f[...]` of a function declared with `snegrealfunctions` or `snegcomplexfunctions`;
  - the head of an expression with the attribute `NumericFunction`, such as `Plus`, `Times`, `Power`, `Sin`, `Exp`, `Conjugate`, `Abs`. SNEG gives this attribute also to `KroneckerDelta`, `UnitStep`, and `levicivita`.
- Expectation values `avg[...]` count as scalars, irrespective of their argument.
- Operators, Grassmann numbers (`sneggrassmanconstants`), undeclared symbols, and functions without the `NumericFunction` attribute, e.g. `DiracDelta`, are not scalars. Symbolic sums `sum[...]` are scalars only if their summand is a scalar.
- `isnumericQ` is used by `nc`, `dd`, `vev`, `conj`, and all functions declared with `sneglinearoperator`, `sneglinearoperatorFirst`, and `snegsesquilinearoperator` to decide which factors are pulled out. A symbol that is not declared as a constant is treated as an operator of unknown type: it stays inside the products, which are then not brought to normal order.

## Developer Notes
- The test of each subexpression uses the upvalue `snegnonopQ[x]`, which the declaration functions set to `True`. To declare further scalars, define `snegnonopQ[x] = True` (for a protected symbol, e.g. `snegnonopQ[DiracDelta] = True`) or `x /: snegnonopQ[x] = True`; for a function `f`, `snegnonopQ[f] = True` makes all applications `f[...]` scalars. This works also inside products and powers, e.g. `isnumericQ[3 x^2]`.
- A definition `isnumericQ[x] ^= True` only applies to the symbol `x` itself, not to expressions containing it.
- The wrapper `scalar[x]` marks scalars without any test; see `scalar`.

## Examples
### Basic Examples
```wl
snegrealconstants[t];
snegcomplexconstants[z];
snegfermionoperators[c];
{isnumericQ[t], isnumericQ[2 t + I z^2], isnumericQ[u], isnumericQ[c[CR, 1, UP]]}
```
Scalars are factored out of products, undeclared symbols are not:
```wl
{nc[c[AN, 1, UP], t c[CR, 1, UP]], nc[c[AN, 1, UP], u c[CR, 1, UP]]}
```

### Scope
Numbers, mathematical constants, and functions with the attribute `NumericFunction` of scalars:
```wl
snegrealconstants[t];
{isnumericQ[2], isnumericQ[Pi], isnumericQ[Sqrt[t]], isnumericQ[Exp[I t]], isnumericQ[Sin[u]]}
```
---
Declared functions, summation indexes, Kronecker deltas, and step functions:
```wl
snegrealfunctions[eps];
snegcomplexfunctions[V];
snegfreeindexes[k];
{isnumericQ[eps[k]], isnumericQ[V[k] Conjugate[V[k]]], isnumericQ[KroneckerDelta[k, 1]], isnumericQ[UnitStep[k]], isnumericQ[g[k]]}
```
---
Expectation values `avg[...]` are scalars; Grassmann numbers are not:
```wl
snegfermionoperators[c];
sneggrassmanconstants[x];
{isnumericQ[avg[nc[c[CR, 1, UP], c[AN, 1, UP]]]], isnumericQ[x]}
```

### Applications
Declare a new scalar for an existing protected function. `DiracDelta` is then factored out of products:
```wl
snegfermionoperators[c];
snegrealconstants[x, y];
snegnonopQ[DiracDelta] = True;
{isnumericQ[DiracDelta[x - y]], nc[c[AN, 1, UP], DiracDelta[x - y] c[CR, 1, UP]]}
```
---
Declare a symbol as a scalar with an upvalue:
```wl
snegfermionoperators[c];
w /: snegnonopQ[w] = True;
{isnumericQ[3 w^2], nc[c[AN, 1, UP], w^2 c[CR, 1, UP]]}
```

### Properties & Relations
`isnumericQ` differs from `NumericQ`, which is `False` for symbolic parameters:
```wl
snegrealconstants[t];
{NumericQ[t], isnumericQ[t]}
```
---
`isnumericQ` determines which factors `conj` conjugates with `Conjugate`; real constants are invariant:
```wl
snegrealconstants[t];
snegcomplexconstants[z];
snegfermionoperators[c];
conj[(t + z) c[CR, 1, UP]]
```

### Possible Issues
An upvalue for `isnumericQ` itself applies only to the symbol, not to expressions containing it; declare scalars with `snegnonopQ` instead:
```wl
isnumericQ[v] ^= True;
{isnumericQ[v], isnumericQ[2 v], isnumericQ[v^2]}
```
---
Undeclared symbols are operators. A product with an undeclared parameter is not normal ordered:
```wl
snegfermionoperators[c];
nc[c[AN, 1, UP], U c[CR, 1, UP]]
```
