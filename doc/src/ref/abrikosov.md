---
keywords: Abrikosov pseudofermions, pseudofermion representation, slave fermions, spinons, Kondo model, Coqblin-Schrieffer
see also: schwingerbosons, holsteinprimakoff, jordanwigner, spinxyz, projector1
---
# abrikosov

## Usage
- `abrikosov[expr, S -> f]` replaces the spin operators `S` in *expr* by the Abrikosov pseudofermion representation with the fermions `f`.

## Details
- The representation is $\mathbf S_j = \sum_{mm'} f^\dagger_{jm} \mathbf S_{mm'} f_{jm'}$, i.e. `S[j, t]` is replaced by the corresponding component of `spinxyz[f[j]]` (with $S^\pm = S^x \pm i S^y$). For spin 1/2, $S^+_j = f^\dagger_{j\uparrow} f_{j\downarrow}$, $S^z_j = \frac12 (n_{j\uparrow} - n_{j\downarrow})$.
- The fermion must have the same spin as the spin operators: `snegfermionoperators[{f, s}]` for `snegspinoperators[{S, s}]` (spin 1/2: `snegfermionoperators[f]`). Otherwise the message `abrikosov::spin` is issued and `$Failed` is returned.
- The representation preserves the algebra, but is faithful only in the subspace with one fermion per site, $\sum_m n_{jm} = 1$; the constraint is not imposed. For spin 1/2, the projector onto this subspace is `projector1[f[j]]`.
- The result is expanded.

## Examples
### Basic Examples
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[f];
abrikosov[{S[1, 3], S[1, 4], S[1, 5]}, S -> f]
```

### Scope
Spin 1 with three-flavor fermions:
```wl
snegspinoperators[{T, 1}];
snegfermionoperators[{g, 1}];
abrikosov[T[1, 3], T -> g]
```

### Applications
Heisenberg exchange of two spins 1/2 represented by pseudofermions. In the subspace with one fermion per site, the spectrum is the singlet $-3/4$ and the triplet $1/4$:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[f];
h = abrikosov[spinspin[S[1], S[2]], S -> f];
Select[{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[h, qszbasisvc[{f[1], f[2]}]], First[First[#]] == 0 &]
```
The state with one pseudofermion per site at $Q = 0$, $S_z = 0$ includes the doubly occupied states, which are unphysical; the eigenvalues $0, 0$ belong to them.

### Properties & Relations
For spin-1/2 fermions, `abrikosov` gives the fermion spin operators of `spinxyz`:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[f];
abrikosov[spinspin[S[1], S[2]], S -> f] == Expand[spinspin[f[1], f[2]]]
```

### Possible Issues
The fermion must have the spin of the spin operators:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[{g, 1}];
abrikosov[S[1, 3], S -> g]
```
