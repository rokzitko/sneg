---
keywords: mean-field theory, Hartree-Fock, Hartree-Fock-Bogoliubov, HFB, decoupling, Wick's theorem, Stoner, BCS reduction, pairing, exchange, Fock term, Hubbard model, Bose-Hubbard model
see also: meanfieldsolve, avg, gaussianaverage, quadraticform, bdghamiltonian, wick
---
# meanfield

## Usage
- `meanfield[H]` decouples all products of four or more fermion or boson operators in *H* into the Hartree-Fock-Bogoliubov mean-field Hamiltonian, with the averages of operator pairs written as `avg[...]`.
- `meanfield[H, channels]` keeps only the contractions of the given channels: `"Hartree"`, `"Fock"`, `"Pairing"`, `"Normal"`, `"All"`, a list of these, or a selection function.

## Details
- Every product of $2m \ge 4$ ladder operators $o_1 o_2 \cdots o_{2m}$ (as it appears in the normal-ordered *H*) is replaced by
$$\sum_{i<j} s_{ij}\, W(\text{rest}_{ij})\, o_i o_j - (m - 1)\, W(o_1 \cdots o_{2m}),$$
where $W$ denotes the full Wick expansion with the allowed contractions $\langle o_a o_b \rangle$, $\text{rest}_{ij}$ is the product without $o_i$ and $o_j$, and $s_{ij}$ is the fermionic sign of moving $o_i o_j$ to the front. The contractions are written as `avg[nc[oa, ob]]`.
- For four operators this is the familiar decoupling $AB \to \langle A \rangle B + A \langle B \rangle - \langle A \rangle \langle B \rangle$, summed over the three pairings of the operators. For example, with all channels,
$$n_\uparrow n_\downarrow \to \langle n_\downarrow \rangle n_\uparrow + \langle n_\uparrow \rangle n_\downarrow - \langle n_\uparrow \rangle \langle n_\downarrow \rangle - \langle c_\uparrow^\dagger c_\downarrow \rangle c_\downarrow^\dagger c_\uparrow - \ldots + \langle c_\downarrow c_\uparrow \rangle c_\uparrow^\dagger c_\downarrow^\dagger + \ldots$$
- The decoupling is exact for the energy: the average of the mean-field Hamiltonian in any Gaussian (quasi-free) state, with the averages `avg[...]` evaluated in the same state, equals the Wick average of *H*. The stationarity of this energy gives the Hartree-Fock-Bogoliubov equations; `meanfieldsolve` solves them self-consistently.
- The channels select the allowed contractions $\langle o_1 o_2 \rangle$ ($o_1$ to the left of $o_2$ in the product):

| `"Hartree"` | $\langle a^\dagger a \rangle$ of the same mode (equal operator labels) |
| `"Fock"` | $\langle a^\dagger b \rangle$ of different modes, including spin-flip averages $\langle c_\uparrow^\dagger c_\downarrow \rangle$ |
| `"Pairing"` | anomalous averages $\langle a b \rangle$, $\langle a^\dagger b^\dagger \rangle$ |
| `"Normal"` | `"Hartree"` and `"Fock"` |
| `"All"` | all contractions (default) |
| `{ch1, ch2, ...}` | the union of the channels |
| `f` | contractions for which `f[o1, o2]` gives `True` |

- Only operators of the same statistics are contracted (fermions with fermions, bosons with bosons). Terms with zero or two operators are kept unchanged.
- The channel names refer to the operator labels, not to the physical process. In momentum space, the direct (Hartree) term of a density-density interaction appears as $\langle c_{k+q}^\dagger c_k \rangle$ and belongs to `"Fock"`; use `"Normal"` there and impose momentum conservation on the averages afterwards.
- Symbolic sums are decoupled term by term inside the sum.
- Terms with an odd number of operators (`meanfield::odd`), terms containing other operators such as spins or X-operators (`meanfield::op`), and terms that are not products of operators (`meanfield::term`) are kept unchanged with a message. An unknown channel gives `meanfield::channel` and `$Failed`.
- The averages `avg[...]` are numbers for `nc`, `quadraticform`, and `bdghamiltonian`, so the mean-field Hamiltonian can be diagonalized directly. `avg` acts on the canonically ordered expression, so a contraction may appear as `1 - avg[...]` or with a sign.
- The result is expanded.

## Examples
### Basic Examples
Hartree decoupling of the Hubbard interaction $U n_\uparrow n_\downarrow$:
```wl
snegfermionoperators[c];
snegrealconstants[U];
meanfield[U hubbard[c[1]], "Hartree"]
```
All channels (Hartree, Fock, and pairing):
```wl
meanfield[U hubbard[c[1]]]
```

### Scope
#### Channels
The Fock channel contains the spin-flip averages $\langle c_\uparrow^\dagger c_\downarrow \rangle$, which describe transverse magnetization:
```wl
snegfermionoperators[c];
snegrealconstants[U];
meanfield[U hubbard[c[1]], "Fock"]
```
The pairing channel contains the anomalous averages $\langle c_\uparrow^\dagger c_\downarrow^\dagger \rangle$ and $\langle c_\downarrow c_\uparrow \rangle$:
```wl
meanfield[U hubbard[c[1]], "Pairing"]
```
The channels add up to the full decoupling:
```wl
Expand[meanfield[U hubbard[c[1]]] - meanfield[U hubbard[c[1]], {"Hartree", "Fock", "Pairing"}]]
```
#### Selection functions
A function of the two contracted operators selects the contractions; here normal contractions between operators with the same spin, which is equivalent to `"Hartree"` for the on-site interaction:
```wl
snegfermionoperators[c];
snegrealconstants[U];
meanfield[U hubbard[c[1]], Function[{o1, o2}, o1[[1]] =!= o2[[1]] && Last[o1] === Last[o2]]]
```
#### Intersite interactions
The nearest-neighbor interaction $V n_{1\uparrow} n_{2\uparrow}$ gives Hartree terms, the bond (Fock) terms $\langle c_{1\uparrow}^\dagger c_{2\uparrow} \rangle$, and pairing terms:
```wl
snegfermionoperators[c];
snegrealconstants[V];
meanfield[V nc[number[c[1], UP], number[c[2], UP]]]
```
#### Bosons
On-site interaction of bosons, $\frac{U}{2} b^\dagger b^\dagger b b$. The normal contractions give the Hartree-Fock term $2U \langle b^\dagger b \rangle b^\dagger b$ (direct and exchange), and the anomalous averages appear in the full decoupling:
```wl
snegbosonoperators[b];
snegrealconstants[U];
meanfield[U/2 nc[b[CR], b[CR], b[AN], b[AN]], "Normal"]
```
```wl
meanfield[U/2 nc[b[CR], b[CR], b[AN], b[AN]]]
```
#### Momentum space
The BCS reduction of the pairing interaction $-\frac{g}{N_s} \sum_{kp} c_{k\uparrow}^\dagger c_{-k\downarrow}^\dagger c_{-p\downarrow} c_{p\uparrow}$:
```wl
snegfermionoperators[ck];
snegrealconstants[g];
snegmomentumindexes[{k, p}, Ns];
meanfield[-g/Ns sum[nc[ck[CR, k, UP], ck[CR, -k, DO], ck[AN, -p, DO], ck[AN, p, UP]], {k, p}], "Pairing"]
```
With $\Delta = \frac{g}{N_s} \sum_p \langle c_{-p\downarrow} c_{p\uparrow} \rangle$ this is $-\sum_k (\Delta c_{k\uparrow}^\dagger c_{-k\downarrow}^\dagger + \Delta^* c_{-k\downarrow} c_{k\uparrow}) + N_s |\Delta|^2/g$.

### Applications
#### Stoner model of ferromagnetism
With $\langle n_{i\uparrow} \rangle = (n + m)/2$ and $\langle n_{i\downarrow} \rangle = (n - m)/2$, the Hartree decoupling of the Hubbard interaction gives the exchange splitting $U m$ between the two spin bands and the energy $U (m^2 - n^2)/4$ per site:
```wl
snegfermionoperators[c];
snegrealconstants[U, n, m];
mf = meanfield[U hubbard[c[1]], "Hartree"];
Collect[mf /. {avg[number[c[1], UP]] -> (n + m)/2, avg[number[c[1], DO]] -> (n - m)/2}, _nc, Simplify]
```
The self-consistency condition $m = \sum_k [f(\epsilon_k - Um/2) - f(\epsilon_k + Um/2)]/N_s$ has a solution $m \neq 0$ when $U \rho(\epsilon_F) > 1$ (Stoner criterion), where $\rho$ is the density of states per spin and site. `meanfieldsolve` solves the equations for finite clusters.
#### Antiferromagnetic Hartree-Fock for the Hubbard dimer
The Hartree-Fock Hamiltonian of the half-filled Hubbard dimer at $U = 4t$ is solved with `meanfieldsolve`. The broken-symmetry solution has the staggered moment $m = \sqrt3/2$ and the energy $-t/2$, above the exact ground-state energy $(U - \sqrt{U^2 + 16 t^2})/2 \approx -0.83 t$:
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]] + 4 (hubbard[c[1]] + hubbard[c[2]]);
sol = meanfieldsolve[meanfield[H, "Hartree"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2];
{sol["Energy"], (avg[number[c[1], UP]] - avg[number[c[1], DO]]) /. sol["Averages"]}
```

### Properties & Relations
The average of the mean-field Hamiltonian in a Gaussian state equals the Wick average of $H$; here for the Hubbard dimer in the thermal state of a quadratic reference Hamiltonian $H_0$:
```wl
snegfermionoperators[c];
H0 = -hop[c[1], c[2]] + 1/5 number[c[1]];
Hint = 2 (hubbard[c[1]] + hubbard[c[2]]);
mf = meanfield[Hint];
rules = Map[# -> gaussianaverage[First[#], H0, {c[1], c[2]}, 1/3] &, Union[Cases[mf, _avg, Infinity]]];
{gaussianaverage[mf /. rules, H0, {c[1], c[2]}, 1/3], gaussianaverage[Hint, H0, {c[1], c[2]}, 1/3]}
```
---
The mean-field Hamiltonian is quadratic, with the averages treated as numbers:
```wl
snegfermionoperators[c];
snegrealconstants[U];
quadraticform[meanfield[U hubbard[c[1]], {"Hartree", "Pairing"}], {c[1]}] // First // MatrixForm
```
---
Higher products are decoupled as well; for six operators the constant term is $-2W$:
```wl
snegspinlessfermionoperators[f];
meanfield[nc[number[f[1]], number[f[2]], number[f[3]]], "Hartree"]
```

### Possible Issues
In momentum space, the direct term of the interaction contracts operators with different momentum labels, which is not a `"Hartree"` contraction by the operator labels; the result vanishes:
```wl
snegfermionoperators[ck];
snegrealconstants[U];
snegmomentumindexes[{k, p, q}, Ns];
Hq = U/Ns sum[nc[ck[CR, k + q, UP], ck[AN, k, UP], ck[CR, p - q, DO], ck[AN, p, DO]], {k, p, q}];
meanfield[Hq, "Hartree"]
```
Use `"Normal"` and impose momentum conservation on the averages afterwards:
```wl
meanfield[Hq, "Normal"]
```
---
Products with spin operators are not decoupled:
```wl
snegfermionoperators[c];
snegspinoperators[{S, 1/2}];
meanfield[nc[S[1, 3], number[c[1], UP], number[c[1], DO]]]
```
