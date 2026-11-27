---
keywords: Berezin integral, Grassmann integration, fermionic path integral, Gaussian integral
see also: sneggrassmanconstants, grassmanQ, conj, nc
---
# int

## Usage
- `nc[int[x], expr]` represents the Berezin integral $\int d\xi\, \mathrm{expr}$ over the Grassmann number $\xi$ = *x*.
- `int[x, y, ...]` in a product is `int[x], int[y], ...`.

## Details
- The Berezin integral is defined by $\int d\xi\, 1 = 0$ and $\int d\xi\, \xi = 1$. In `nc[..., int[x], b, ...]`, the integral acts on the factors to its right: if they do not contain $\xi$ (at the first level of the product), the result is zero; otherwise $\xi$ is moved to the front with the sign of the anticommuting factors it passes (fermions, Majorana fermions, Grassmann numbers), and it is removed together with `int[x]`.
- *x* must be declared with `sneggrassmanconstants`; the conjugate `conj[x]` can also be integrated.
- `nc[int[x]]` alone is 0. `int` acts only inside `nc`.
- The integrand must be written as a polynomial in the Grassmann numbers: functions such as `Exp` are not expanded. Use the finite Taylor series, e.g. $e^{-a\bar\xi\xi} = 1 - a\bar\xi\xi$.
- The measures `int[...]` are not counted as anticommuting factors. For a single pair of variables this reproduces the standard results, e.g. $\int d\bar\xi\, d\xi\, e^{-a\bar\xi\xi} = a$; for several variables, see Possible Issues.

## Examples
### Basic Examples
```wl
sneggrassmanconstants[x, y];
{nc[int[x], 1], nc[int[x], x], nc[int[x], y, x], nc[int[x], y]}
```

### Applications
Gaussian integral over a pair of conjugate Grassmann numbers, $\int d\bar\xi\, d\xi\, e^{-a\bar\xi\xi} = a$:
```wl
sneggrassmanconstants[x];
snegrealconstants[a];
nc[int[conj[x]], int[x], 1 - a nc[conj[x], x]]
```

### Possible Issues
`Exp` is not expanded, so the integral is evaluated as if the integrand did not contain the variables:
```wl
sneggrassmanconstants[x];
snegrealconstants[a];
nc[int[conj[x]], int[x], Exp[-a nc[conj[x], x]]]
```
---
For several variables, the sign depends on how the integrals are grouped. The flattened product and the nested evaluation, $\int d\xi \left(\int d\eta\, \xi\eta\right)$, differ:
```wl
sneggrassmanconstants[x, y];
{nc[int[x], int[y], x, y], nc[int[x], nc[int[y], x, y]]}
```
The Gaussian integral over two pairs, with the measure $d\bar\xi_1 d\xi_1 d\bar\xi_2 d\xi_2$, gives $-\det A$ instead of the standard $\det A$:
```wl
sneggrassmanconstants[x1, x2];
snegrealconstants[a11, a12, a21, a22];
S = a11 nc[conj[x1], x1] + a12 nc[conj[x1], x2] + a21 nc[conj[x2], x1] + a22 nc[conj[x2], x2];
Expand[nc[int[conj[x1]], int[x1], int[conj[x2]], int[x2], 1 - S + pow[S, 2]/2]]
```
