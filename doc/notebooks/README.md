# Second Quantization with SNEG: teaching notebooks

Download a notebook, open it in Mathematica, and evaluate its cells in order
in a fresh kernel with SNEG 2.2 installed. The saved outputs are the evaluated
examples from the [manual](../manual/sneg-manual.tex).

| Notebook | What to check |
|---|---|
| [Direct and exchange contractions](exchange.nb) | Why the exchange term requires equal spins |
| [The SSH chain](ssh.nb) | Which off-diagonal Bloch element carries the intercell phase |
| [The Dirac point of graphene](graphene.nb) | Cancellation of the three hopping paths at a valley |
| [The Rashba chain](rashba.nb) | Spin splitting under momentum reversal |
| [Momentum transfer in the Holstein model](holstein.nb) | Electron recoil on phonon emission |
| [The displaced impurity oscillator](lang-firsov.nb) | Convergence of a truncated phonon calculation to the exact relaxation energy |
| [Superexchange in the Hubbard dimer](hubbard-dimer.nb) | Singlet–triplet splitting from exact diagonalization and from projection |
| [Spectral poles of the Hubbard atom](atomic-propagator.nb) | Pole positions, thermal residues and the total spectral weight |
| [The finite-temperature BCS gap equation](gap.nb) | The anomalous contraction that enters self-consistency |
| [Singlet and doublet in a superconducting dot](andreev.nb) | A ground-state parity crossing in the zero-bandwidth model |
| [Gap closings of the Kitaev chain](kitaev.nb) | The two momenta at which the bulk gap can close |
| [Spin waves in a staggered field](magnons.nb) | Bosonic Bogoliubov frequencies against the spin-wave dispersion |
| [The Jordan–Wigner transformation of the XXZ chain](xxz.nb) | Equality of the open-chain spin and fermion spectra |
| [From the Anderson model to exchange](anderson.nb) | The impurity singlet–triplet gap at small hybridization |
| [Bonding and antibonding spectral weight](lehmann.nb) | Why the integrated off-diagonal weight vanishes |

Rebuild with `wolframscript -file doc/manual/build-notebooks.wl` after building
the manual. To reproduce a saved output from the notebook itself, for example:

```sh
wolframscript -file doc/manual/test-notebook.wl doc/notebooks/atomic-propagator.nb
```

SNEG documentation project · [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
