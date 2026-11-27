---
keywords: Hubbard X-operators, Hubbard operators, atomic limit, effective Hamiltonian, Schrieffer-Wolff, t-J model
see also: xtofermions, xoperator, xtospin, snegxoperators, schriefferwolff
---
# fermionstox

## Usage
- `fermionstox[expr, c -> X]` replaces the spin-1/2 fermion operators `c[CR|AN, j..., σ]` in *expr* by the X-operators `X[j..., a, b]` of the Hubbard atom.

## Details
- The replacement is
  $$c^\dagger_\uparrow = X^{\uparrow 0} + X^{2\downarrow}, \quad c^\dagger_\downarrow = X^{\downarrow 0} - X^{2\uparrow}, \quad c_\uparrow = X^{0\uparrow} + X^{\downarrow 2}, \quad c_\downarrow = X^{0\downarrow} - X^{\uparrow 2},$$
  with $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow |0\rangle$ (see `xoperator`). The products are then reduced with the X-operator algebra.
- `c` must be a spin-1/2 fermion operator (message `fermionstox::spin`), and `X` must be declared with `snegxoperators[X, "HubbardAtom"]` (message `xtofermions::states`); otherwise `$Failed` is returned.
- `fermionstox` is the inverse of `xtofermions`. A typical use is to express effective Hamiltonians from `schriefferwolff`, which contain projectors onto local occupancies, in terms of X-operators, and then in terms of spins with `xtospin`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
{fermionstox[c[CR, 1, UP], c -> X], fermionstox[c[AN, 1, DO], c -> X]}
```

### Scope
Number operator, Hubbard interaction, and spin of a level in terms of X-operators:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
fermionstox[{number[c[1]], hubbard[c[1]], spinz[c[1]], spinplus[c[1]]}, c -> X]
```
---
Hopping between two sites:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
fermionstox[hop[c[1], c[2]], c -> X]
```

### Applications
Superexchange of the half-filled Hubbard dimer from the Schrieffer-Wolff transformation, converted to spin operators: $\frac{4t^2}{U} (\mathbf S_1 \cdot \mathbf S_2 - \frac14)$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
snegxoperators[X, "HubbardAtom"];
snegspinoperators[{S, 1/2}];
heff = schriefferwolff[U (hubbard[c[1]] + hubbard[c[2]]), -t hop[c[1], c[2]], nc[projector1[c[1]], projector1[c[2]]], 2];
Expand[xtospin[fermionstox[heff[[3]], c -> X], X -> S]]
```

### Properties & Relations
`fermionstox` and `xtofermions` are inverse to each other:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
h = spinspin[c[1], c[2]] + hop[c[1], c[2]];
Expand[xtofermions[fermionstox[h, c -> X], X -> c] - h]
```

### Possible Issues
The fermions must have spin 1/2:
```wl
snegbosonoperators[b];
snegxoperators[X, "HubbardAtom"];
fermionstox[b[CR], b -> X]
```
