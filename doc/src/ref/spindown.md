---
keywords: spin lowering, spin multiplet, total spin, Clebsch-Gordan, highest-weight state
see also: spindownvc, spinminus, fixspin, manyspin, qsbasis
---
# spindown

## Usage
- `spindown[v]` applies the total spin-lowering operator $S^-$ of the levels in the state *v* (in the creation-operator representation) and normalizes the result.

## Details
- The levels are determined from the operators `c[t, j..., σ]` that appear in *v*; the total $S^- = \sum_j S^-_{c[j]}$ of these levels (see `spinminus`) is applied. Parts that annihilate the vacuum are dropped (`zeroonvac`), and the result is divided by its norm.
- *v* is a creation-operator string or a linear combination of such strings, optionally followed by `VACUUM`. If $S^- v = 0$, the result is 0.
- Starting from the highest-weight state $|S, S_z = S\rangle$ of a multiplet, repeated application gives the states $|S, S_z\rangle$ with the standard phases, since all matrix elements of $S^-$ are positive. `fixspin` applies it the required number of times.
- Only spin-1/2 fermions are supported.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spindown[c[CR, 1, UP]]
```
The triplet states of two electrons from $|{\uparrow\uparrow}\rangle$:
```wl
NestList[spindown, nc[c[CR, 1, UP], c[CR, 2, UP]], 3]
```

### Properties & Relations
The singlet is annihilated:
```wl
snegfermionoperators[c];
spindown[nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]]]
```
---
`spindownvc` does the same in the occupation-number representation:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vc2ops[spindownvc[ops2vc[nc[c[CR, 1, UP], c[CR, 2, UP]]]]] == spindown[nc[c[CR, 1, UP], c[CR, 2, UP]]]
```
