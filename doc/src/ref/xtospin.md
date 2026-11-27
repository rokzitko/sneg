---
keywords: Hubbard X-operators, spin operators, local moment, singly occupied subspace, Heisenberg model, Kondo model, effective Hamiltonian
see also: fermionstox, xtofermions, xoperator, snegxoperators, snegspinoperators, schriefferwolff
---
# xtospin

## Usage
- `xtospin[expr, X -> S]` replaces the X-operators of singly occupied sites, $X^{\sigma\sigma'}$ with $\sigma, \sigma' \in \{\uparrow, \downarrow\}$, by spin-1/2 operators `S`.

## Details
- The replacement is
  $$X^{\uparrow\uparrow} = \frac12 + S^z, \quad X^{\downarrow\downarrow} = \frac12 - S^z, \quad X^{\uparrow\downarrow} = S^+, \quad X^{\downarrow\uparrow} = S^-,$$
  for each site `X[j..., a, b]` → `S[j..., t]`. It is valid in the subspace where the site is singly occupied ($X^{\uparrow\uparrow} + X^{\downarrow\downarrow} = 1$).
- Terms that contain X-operators involving the states `"0"` or `"2"` are dropped, and the message `xtospin::out` lists them. If the expression is projected onto single occupancy (e.g. an effective Hamiltonian from `schriefferwolff` with `projector1`), no such terms occur.
- `X` must be declared with `snegxoperators[X, "HubbardAtom"]` (message `xtofermions::states`), and `S` with `snegspinoperators` (message `xtospin::spinop`); otherwise `$Failed` is returned.
- Use `fermionstox` first to convert fermion expressions to X-operators.

## Examples
### Basic Examples
```wl
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
xtospin[X[1, "UP", "DO"] + X[1, "DO", "DO"], X -> S]
```

### Scope
The exchange interaction of two fermion levels in terms of spin operators:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
xtospin[fermionstox[spinspin[c[1], c[2]], c -> X], X -> S]
```

### Applications
Anderson impurity to Kondo model: the second-order Schrieffer-Wolff term at single occupancy of the impurity, in terms of the impurity spin $\mathbf S$ (exchange and potential scattering):
```wl
snegfermionoperators[a, d];
snegrealconstants[ed, U, v];
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
heff = schriefferwolff[ed number[d[]] + U hubbard[d[]], v hop[a[], d[]], projector1[d[]], 2];
hk = Expand[xtospin[fermionstox[heff[[3]], d -> X], X -> S]]
```
The coefficient of $S^z n_{a\uparrow}$ is $J/2$ with the Kondo exchange $J = -2v^2 U/[\epsilon_d (\epsilon_d + U)]$, which is antiferromagnetic for $-U < \epsilon_d < 0$:
```wl
Simplify[Coefficient[hk, nc[S[3], a[CR, UP], a[AN, UP]]]]
```

### Properties & Relations
The spin operators reproduce the X-operator algebra in the singly occupied subspace, e.g. $X^{\uparrow\downarrow} X^{\downarrow\uparrow} = X^{\uparrow\uparrow}$:
```wl
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
{xtospin[nc[X[1, "UP", "DO"], X[1, "DO", "UP"]], X -> S], xtospin[X[1, "UP", "UP"], X -> S]}
```

### Possible Issues
Terms outside the singly occupied subspace are dropped with a message:
```wl
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
xtospin[X[1, "2", "UP"] + X[1, "UP", "UP"], X -> S]
```
---
The target must be a spin operator:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
xtospin[X[1, "UP", "UP"], X -> c]
```
