---
keywords: expectation value, matrix element, braket, creation operator representation, state
see also: braketop, scalarproductop, normop, vev, expvvc
---
# expvop

## Usage
- `expvop[op, a]` gives $\langle a | \mathit{op} | a \rangle$ for the operator *op* and the state $|a\rangle = a|0\rangle$ in the creation-operator representation.

## Details
- `expvop[op, a]` is `braketop[a, op, a]`, i.e., `vev[nc[conj[a], op, a]]`. The vacuum is the one set by `ordering`.
- The state is not normalized: for a normalized expectation value, divide by `scalarproductop[a, a]` or use a normalized state.
- `expvop` is `Listable` in both arguments.

## Examples
### Basic Examples
Total spin $\mathbf S^2$ of the normalized singlet and triplet states of two electrons:
```wl
snegfermionoperators[c];
s = (nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];
t0 = (nc[c[CR, 1, UP], c[CR, 2, DO]] + nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];
stot = spinss[c[1]] + spinss[c[2]] + 2 spinspin[c[1], c[2]];
{expvop[stot, s], expvop[stot, t0]}
```

### Scope
Lists of operators:
```wl
snegfermionoperators[c];
expvop[{number[c[1]], number[c[2]], spinz[c[1]]}, c[CR, 1, UP]]
```
---
The state is not normalized:
```wl
snegfermionoperators[c];
psi = c[CR, 1, UP] + c[CR, 2, UP];
{expvop[number[c[1]], psi], expvop[number[c[1]], psi]/scalarproductop[psi, psi]}
```
---
The Fermi sea is the vacuum for `ordering[f] = SEA`. The occupation of the level $-1$ in the Fermi sea (state 1) and with a spin-up hole in this level:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
{expvop[number[f[-1]], 1], expvop[number[f[-1]], f[AN, -1, UP]]}
```

### Applications
Energy of the Heitler-London (singly occupied) singlet and of the doubly occupied ionic states in the Hubbard dimer:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
s = (nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];
d = (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[CR, 2, UP], c[CR, 2, DO]])/Sqrt[2];
{expvop[H, s], expvop[H, d], braketop[d, H, s]}
```
