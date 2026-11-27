---
keywords: expectation value, average, mean field, order parameter, symbolic average
see also: meanfield, meanfieldsolve, gaussianaverage, spectralmoments, greensfunction, vev
---
# avg

## Usage
- `avg[x]` represents the expectation value $\langle x \rangle$ of the operator expression *x*.

## Details
- `avg` is the symbolic expectation value used by `meanfield`, `meanfieldsolve`, `spectralmoments`, and `greensfunction`. It has no definite value; numerical values are obtained with `meanfieldsolve`, or by replacing the averages with the results of `gaussianaverage` or `thermalaverage`.
- `avg` is linear: c-numbers are taken out, $\langle a x + b y \rangle = a \langle x \rangle + b \langle y \rangle$, and the average of a c-number is the c-number.
- The argument is the canonically ordered operator expression, as produced by `nc`. Therefore $\langle c c^\dagger \rangle$ evaluates to $1 - \langle c^\dagger c \rangle$ for a fermion, and different orderings of the same product are identified.
- The average of a single fermion operator is 0. Averages of other odd products are kept.
- Complex conjugation gives the average of the Hermitian conjugate: $\langle x \rangle^* = \langle x^\dagger \rangle$.
- `avg[...]` is treated as a number in operator products (`isnumericQ` gives `True`), so mean-field Hamiltonians are quadratic operators for `nc`, `quadraticform`, and `bdghamiltonian`.
- Averages of symbolic sums are sums of averages: `avg[sum[x, {k}]]` is `sum[avg[x], {k}]`.
- In `StandardForm`, `avg[x]` is displayed as $\langle x \rangle$.

## Examples
### Basic Examples
The average is linear and acts on the canonically ordered expression:
```wl
snegfermionoperators[c];
{avg[3 number[c[1], UP] + 2], avg[nc[c[AN, 1, UP], c[CR, 1, UP]]]}
```
---
Complex conjugation:
```wl
snegfermionoperators[c];
Conjugate[avg[nc[c[CR, 1, UP], c[AN, 2, UP]]]]
```

### Scope
Single fermion operators average to zero; boson operators do not:
```wl
snegfermionoperators[c];
snegbosonoperators[b];
{avg[c[CR, 1, UP]], avg[b[AN]]}
```
---
Averages are numbers in operator products:
```wl
snegfermionoperators[c];
nc[c[CR, 1, UP], 2 avg[number[c[1], DO]] c[AN, 1, UP]]
```
---
Averages of sums:
```wl
snegspinlessfermionoperators[f];
snegfreeindexes[k];
avg[sum[number[f[k]], {k}]]
```

### Applications
The averages in a mean-field Hamiltonian are replaced by numbers from `gaussianaverage`; for the Hartree decoupling of $U n_\uparrow n_\downarrow$ in the state of a level at energy $-1/2$ and temperature $1/4$:
```wl
snegfermionoperators[d];
mf = meanfield[2 hubbard[d[]], "Hartree"];
rules = Map[# -> gaussianaverage[First[#], -1/2 number[d[]], {d}, 1/4] &, Union[Cases[mf, _avg, Infinity]]];
mf /. rules
```

### Possible Issues
Because the argument is canonically ordered, an average may appear with a sign or a constant. When initial values are given to `meanfieldsolve`, use the averages as they appear in the mean-field Hamiltonian; rules for multiples such as `-avg[x] -> v` are accepted:
```wl
snegfermionoperators[c];
{avg[nc[c[AN, 1, UP], c[AN, 1, DO]]], avg[nc[c[AN, 1, DO], c[CR, 1, DO]]]}
```
