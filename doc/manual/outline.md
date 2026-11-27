# SNEG User's Manual: outline (Checkpoint B)

Working title: *Second Quantization with SNEG: A Practical Guide for Solid-State
Theory*. Audience: MSc and PhD students in solid-state theory, researchers.
Classic textbook layout (KOMA-Script `scrbook`, A4/B5, one column with margin
notes), about 370 pages. Page budgets in brackets.

Every chapter of Parts I and II ends with exercises; full solutions are in
Appendix C. All code in the manual is executed when the manual is built, and
the outputs are typeset in physics notation (`snegtex`) and cached, as for the
help pages. Problems taken from textbooks and problem collections are
reformulated and cited.

## Front matter [10]

- Preface: purpose, scope, how to read the book, citation of SNEG.
- Conventions and notation (overview; details in Appendix A).
- Installation and first session.

## Part I. Tutorial [84]

### 1. Getting started [10]
1.1 What SNEG does: canonical order, automatic signs, symbolic sums.
1.2 Installation, loading, help pages, versions.
1.3 A first calculation: the Hubbard atom from operators to spectrum.
1.4 Reading SNEG output; `FullForm`; `snegtex` for papers.
1.5 How the book is organized.

### 2. Operators and their algebra [14]
2.1 Declaring operators: fermions with spin and orbital indexes, spinless
    fermions, bosons, Majorana fermions, spins, Grassmann numbers.
2.2 Declaring parameters: real, complex, integer constants and functions.
2.3 The product `nc`: linearity, associativity, canonical order.
2.4 Normal order and the orderings EMPTY, SEA, NONE (particle-hole
    transformation of the Fermi sea).
2.5 Commutators, anticommutators, nested commutators; `conj`, `pow`.
2.6 Spin operators: size, normal form, Cartesian and spherical components.
2.7 Hubbard X-operators.

### 3. Building Hamiltonians [12]
3.1 Number operators, hopping, Coulomb interaction, exchange.
3.2 Tight-binding models on finite clusters.
3.3 Superconducting pairing and anomalous terms; Nambu spinors.
3.4 Projectors, local Hilbert spaces, constraints.
3.5 Model library: `hamiltonian` and common models.

### 4. Symbolic sums and momentum space [14]
4.1 `sum`: dummy indexes, automatic renaming, products of sums.
4.2 Kronecker deltas and their elimination.
4.3 Lattices and momenta; Fourier transformation; momentum conservation.
4.4 Bloch Hamiltonians and band structures.
4.5 Finite periodic and twisted clusters.

### 5. States, bases, and exact diagonalization [18]
5.1 Occupation-number states, `vc` and operator forms, sign conventions.
5.2 Applying operators to states; scalar products, expectation values.
5.3 Symmetries: charge Q, spin projection Sz, total spin S (Clebsch-Gordan),
    isospin, parity, momentum.
5.4 Bases and matrices in symmetry sectors; block diagonalization.
5.5 Spectra, ground states, correlation functions, thermodynamics.
5.6 Basis transformations: left-right (even-odd) combinations,
    particle-hole transformation.

### 6. Expectation values and Wick's theorem [8]
6.1 Vacuum and Fermi-sea expectation values.
6.2 Contractions and Wick's theorem.
6.3 Gaussian (quadratic) states at finite temperature.

### 7. Working efficiently [8]
7.1 Simplification strategies; `Expand`, `sumSimplifyKD`, `SnegSimplify`.
7.2 Performance: size of expressions, sparse matrices, order of operations.
7.3 Common pitfalls (Times vs nc, undeclared symbols, levels vs operators).
7.4 Exporting results: LaTeX, numerical codes.

## Part II. Applications in solid-state theory [159]

### 8. Electrons in crystals [22]
8.1 Second quantization of the electron gas; Coulomb interaction in
    momentum space; Hartree-Fock energy (exchange energy, Wigner-Seitz
    radius).
8.2 Tight binding: chain, square and cubic lattices, density of states,
    van Hove singularities.
8.3 Lattices with a basis: SSH chain, graphene, Dirac cones.
8.4 Spin-orbit coupling: Rashba chain, Kane-Mele model.
8.5 Topology: Berry phase and winding numbers from Bloch Hamiltonians.
8.6 Electrons in a magnetic field: Peierls substitution, Hofstadter
    cluster.

### 9. Lattice vibrations and the electron-phonon interaction [20]
9.1 Quantization of lattice vibrations; phonon operators.
9.2 Displaced oscillator and the Lang-Firsov transformation.
9.3 Holstein and Su-Schrieffer-Heeger models; polaron in the atomic limit
    and on a dimer (finite phonon bases).
9.4 Fröhlich Hamiltonian and the phonon-mediated attraction (canonical
    transformation).
9.5 Jahn-Teller effect: E⊗e problem.

### 10. Correlated electrons [27]  (sample chapter, Checkpoint C)
10.1 The Hubbard model; atomic limit; particle-hole and spin symmetries.
10.2 The Hubbard dimer: exact solution by symmetry sector.
10.3 Strong coupling: Schrieffer-Wolff to the Heisenberg and t-J models;
     three-site terms; fourth order and ring exchange on a plaquette.
10.4 Mott insulator vs band insulator; charge gap of small clusters.
10.5 Green's functions of the Hubbard atom; Hubbard-I approximation.
10.6 Mean-field theory: Stoner instability, antiferromagnetic Hartree-Fock.
10.7 Slave particles and constraints (X-operators, Gutzwiller projector).
10.8 Multi-orbital interactions: Kanamori Hamiltonian, Hund's coupling
     and the atomic multiplets of a three-orbital shell.

### 11. Superconductivity [24]
11.1 The reduced BCS Hamiltonian; Cooper problem.
11.2 Bogoliubov transformation; quasiparticles; coherence factors.
11.3 Gap equation at zero and finite temperature.
11.4 Nambu formalism and Bogoliubov-de Gennes equations.
11.5 Andreev bound states in junctions; phase-biased junctions; Josephson
     current.
11.6 Yu-Shiba-Rusinov states: magnetic impurity in a superconductor.
11.7 Kitaev chain and Majorana zero modes.
11.8 η-pairing and the SO(4) symmetry of the Hubbard model.

### 12. Magnetism [24]
12.1 Exchange interactions: direct, superexchange, double exchange.
12.2 Heisenberg clusters: dimer, triangle, ring, frustration.
12.3 Spin waves: Holstein-Primakoff, ferromagnetic and antiferromagnetic
     magnons (bosonic Bogoliubov transformation).
12.4 One dimension: Jordan-Wigner transformation, XXZ chain.
12.5 Itinerant magnetism: Stoner model, mean-field phase diagram.
12.6 Single-ion anisotropy and crystal fields: Stevens operators.
12.7 Spin-orbit and anisotropic exchange: Dzyaloshinskii-Moriya
     interaction.

### 13. Quantum impurities and nanostructures [24]
13.1 The Anderson impurity model; atomic limit; local moment.
13.2 Schrieffer-Wolff transformation: Kondo model and potential
     scattering.
13.3 Exact diagonalization with discretized baths; Wilson chain.
13.4 Quantum dots: Coulomb blockade, double dots, singlet-triplet
     physics.
13.5 Impurities in superconductors; superconducting islands.
13.6 From SNEG to NRG Ljubljana: defining models and operators for
     numerical renormalization group calculations.

### 14. Fermi liquids, Green's functions, and response [18]
14.1 Green's functions: equations of motion, Lehmann representation.
14.2 Spectral moments and sum rules.
14.3 Linear response: Kubo formula, susceptibilities of small systems.
14.4 Lindhard function and RPA in symbolic form.
14.5 Fermi-liquid parameters from perturbation theory (second-order
     self-energy structure).

## Part III. Reference [80]

Generated from the help-page sources (`doc/src/ref`): for every function,
usage, details, options, and key examples, grouped as in the guide page.
Index of functions.

## Appendices [44]

A. Conventions and notation: operator indexes, ordering, Fourier
   transformation, Nambu spinors, Green's functions [6].
B. Developer's guide: internal representation, extension points (`ordering`,
   `acmt`, `cmt`, `snegOrderedQ`, `isnumericQ`, linear operators),
   implementing new algebras, testing, legacy functions [12].
C. Solutions to the exercises [24].
D. Bibliography and index [2+].

## Research connections

Short "Further reading" boxes cite the maintainer's work where a section
uses the same methods. Papers and the sections they connect to:

| Paper ([maintainer's bibliography](https://auger.ijs.si/nano/biblio/biblio.html)) | Sections |
|---|---|
| T. Iličin, R. Žitko, Quasiparticle-resolved variational theory of Andreev spin qubits, arXiv:2610.08557 | 11.5, 13.5 |
| Á. Bácsi, T. Iličin, R. Žitko, Systematic Schrieffer-Wolff-transformation approach to Josephson junctions: quasiparticle effects and Josephson harmonics, Phys. Rev. B 113, 064501 (2026) | 10.3, 11.5, 13.2 |
| T. Iličin, R. Žitko, Variational solution of the superconducting Anderson impurity model and the band-edge singularity phenomena, SciPost Phys. 19, 006 (2025) | 11.6, 13.5 |
| L. Pavešić, R. Aguado, R. Žitko, Strong-coupling theory of quantum-dot Josephson junctions: Role of a residual quasiparticle, Phys. Rev. B 109, 125131 (2024) | 11.5, 13.5 |
| L. Pavešić, M. Pita-Vidal, A. Bargerbos, R. Žitko, Impurity Knight shift in quantum dot Josephson junctions, SciPost Phys. 15, 070 (2023) | 13.5 |
| J. Vučičević, R. Žitko, Electrical conductivity in the Hubbard model: Orbital effects of magnetic field, Phys. Rev. B 104, 205101 (2021) | 8.6, 14.3 |
| A. Horvat, R. Žitko, J. Mravlje, Low-energy physics of three-orbital impurity model with Kanamori interaction, Phys. Rev. B 94, 165140 (2016) | 10.8, 13.4 |
| R. Žitko, R. Peters, T. Pruschke, Properties of anisotropic magnetic impurities on surfaces, Phys. Rev. B 78, 224404 (2008) | 12.6, 13.1 |

The reference pages may cite these papers in "related links" where a
function is used in them (e.g. `schriefferwolff`, `bcs`, `projectorE`).

## Web notebooks (auger.ijs.si)

Twelve to fifteen notebooks derived from Part II, e.g. tight-binding bands,
Holstein polaron, Hubbard dimer, superexchange, BCS gap equation, Andreev
states, Kitaev chain, magnons, Jordan-Wigner XXZ, Anderson to Kondo, double
quantum dot, Green's function of the Hubbard atom, Wick's theorem for the
electron gas.

## Build

LaTeX sources in `doc/manual`, with code blocks in a dedicated environment
that are executed by a script (cached outputs, verified by a test as the help
pages). Licence CC BY 4.0.
