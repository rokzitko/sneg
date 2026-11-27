---
keywords: Hubbard X-operators, fermion representation, t-J model, exact diagonalization, atomic limit
see also: xoperator, fermionstox, xtospin, snegxoperators, makematricesbzvc
---
# xtofermions

## Usage
- `xtofermions[expr, X -> c]` replaces the X-operators `X[j..., a, b]` of the Hubbard atom in *expr* by their fermion representation `xoperator[c[j...], a, b]`.

## Details
- `X` must be declared with `snegxoperators[X, "HubbardAtom"]`, with the states `"0"`, `"UP"`, `"DO"`, `"2"`; otherwise the message `xtofermions::states` is issued and `$Failed` is returned. See `xoperator` for the state conventions.
- The X-operators of the site `X[j...]` become functions of the fermion level `c[j...]`; the result is expanded and normal ordered. This allows exact diagonalization with the fermion bases `qszbasisvc`, ... and `makematricesbzvc`.
- Within the full Fock space of the fermions, the abstract algebra of the X-operators is reproduced exactly: products, commutators, and the (anti)commutation with other fermions.
- `fermionstox` is the inverse transformation.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
xtofermions[X[1, "UP", "0"] + X[1, "2", "2"], X -> c]
```

### Applications
The t-J dimer, $H = -t \sum_\sigma (X^{\sigma 0}_1 X^{0\sigma}_2 + X^{\sigma 0}_2 X^{0\sigma}_1) + J (\mathbf S_1 \cdot \mathbf S_2 - n_1 n_2/4)$, with the spin and density operators written with X-operators. With two electrons, the singlet has the energy $-J$ and the triplet 0; with one electron, the bonding and antibonding states have $\mp t$:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
snegrealconstants[t, J];
sdot[i_, j_] := nc[X[i, "UP", "UP"] - X[i, "DO", "DO"], X[j, "UP", "UP"] - X[j, "DO", "DO"]]/4 + (nc[X[i, "UP", "DO"], X[j, "DO", "UP"]] + nc[X[i, "DO", "UP"], X[j, "UP", "DO"]])/2;
nn[i_] := X[i, "UP", "UP"] + X[i, "DO", "DO"];
hop2 = -t Sum[nc[X[1, s, "0"], X[2, "0", s]] + nc[X[2, s, "0"], X[1, "0", s]], {s, {"UP", "DO"}}];
H = xtofermions[hop2 + J (sdot[1, 2] - nc[nn[1], nn[2]]/4), X -> c];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, qszbasisvc[{c[1], c[2]}]]
```
This is the large-$U$ limit of the Hubbard dimer with $J = 4t^2/U$.

### Properties & Relations
The abstract X-operator algebra agrees with the fermion representation:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
st = {"0", "UP", "DO", "2"};
Union[Flatten[Table[Expand[xtofermions[nc[X[1, a, b], X[1, b2, d]], X -> c] - nc[xoperator[c[1], a, b], xoperator[c[1], b2, d]]], {a, st}, {b, st}, {b2, st}, {d, st}]]]
```
---
`xtofermions` inverts `fermionstox`:
```wl
snegfermionoperators[c];
snegxoperators[X, "HubbardAtom"];
xtofermions[fermionstox[hop[c[1], c[2]], c -> X], X -> c] == Expand[hop[c[1], c[2]]]
```

### Possible Issues
Only X-operators of the Hubbard atom can be converted:
```wl
snegfermionoperators[c];
snegxoperators[Y, {1, 2, 3}, {0, 0, 0}];
xtofermions[Y[1, 1, 2], Y -> c]
```
