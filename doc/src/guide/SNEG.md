---
name: SNEG
keywords: second quantization, many-body physics, fermions, bosons, spin operators, condensed matter, NRG Ljubljana
---
# SNEG: Second Quantization in the Wolfram Language

SNEG performs symbolic calculations with operators of second quantization: fermions, bosons, Majorana fermions, spins, and Hubbard X-operators. Products of operators are brought to a canonical (normal-ordered) form automatically, so that commutators, Hamiltonians, and matrix elements can be computed without sign errors. Functions for symbolic sums and momentum space, symmetry-adapted bases and exact diagonalization, quadratic Hamiltonians and Bogoliubov transformations, effective Hamiltonians, and Green's functions cover the standard calculations of solid-state theory.

SNEG is the symbolic engine of the numerical renormalization group code [NRG Ljubljana](https://github.com/rokzitko/nrgljubljana) for quantum impurity problems. When publishing work that uses SNEG, please cite [R. Žitko, Comput. Phys. Commun. 182, 2259 (2011)](https://doi.org/10.1016/j.cpc.2011.05.013).

## Declaring Operators and Parameters
- `snegfermionoperators` declare fermion operators $c_{i\sigma}$
- `snegspinlessfermionoperators`, `snegbosonoperators`, `snegmajoranaoperators`, `snegspinoperators`, `snegxoperators`, `sneggrassmanconstants`
- `snegrealconstants` declare real parameters such as $t$, $U$
- `snegcomplexconstants`, `snegintegerconstants`, `snegrealfunctions`, `snegcomplexfunctions`
- `snegfreeindexes` declare summation indexes
- `CR`, `AN`, `UP`, `DO`, `VACUUM`

## Operator Algebra
- `nc` noncommutative product of operators
- `komutator` commutator $[a, b]$
- `antikomutator` anticommutator $\{a, b\}$
- `conj` Hermitian conjugation
- `pow` power of an operator expression
- `superkomutator`, `superantikomutator`, `ordering`, `normalorder`, `SnegSimplify`, `snegAssuming`, `snegSeries`

## Building Hamiltonians
- `number` number operator $n_i = \sum_\sigma c^\dagger_{i\sigma} c_{i\sigma}$
- `hop` hopping $\sum_\sigma (c^\dagger_{i\sigma} c_{j\sigma} + \mathrm{h.c.})$
- `hubbard` on-site interaction $n_{i\uparrow} n_{i\downarrow}$
- `spinspin` exchange $\mathbf S_i \cdot \mathbf S_j$
- `holehop`, `anomaloushop`, `spinfliphop`, `twohop`, `matrixhop`, `chargecharge`, `isozsq`, `nambu`, `hamiltonian`
- `projector0`, `projector1`, `projector2`, `projectorUP`, `projectorDO`, `projector02`

## Spin and Isospin
- `spinxyz` spin operators $S^x, S^y, S^z$ of a fermion level
- `spinx`, `spiny`, `spinz`, `spinplus`, `spinminus`, `spinss`, `spinspinz`, `spinspinxy`, `spinspinpm`, `spinspinmp`, `manyspin`, `invertspin`
- `isospin`, `isospinx`, `isospiny`, `isospinz`, `isospinplus`, `isospinminus`, `isospinxyz`, `manyisospin`
- `spincomponents` rewrite in $S^z, S^\pm$ or $S^x, S^y, S^z$
- `spintensor`, `stevens`, `rotatespin`, `levicivita`, `spinclustermatrices`
- `jordanwigner`, `holsteinprimakoff`, `schwingerbosons`, `abrikosov`
- `xoperator`, `xtofermions`, `fermionstox`, `xtospin`

## Sums, Lattices, and Momentum Space
- `sum` symbolic sum over indexes
- `sumExpand`, `sumCollect`, `sumSimplifyKD`, `sumFullSimplify`, `sumThread`, `sumShift`, `sumMomentumCanonical`, `sumAbstractIndex`, `sumNameIndex`, `SimplifyKD`
- `sneglatticeindexes`, `snegmomentumindexes`, `sneglatticevectors`, `latticesize`, `latticeindexQ`, `momentumindexQ`
- `fourier` Fourier transformation to momentum space
- `blochhamiltonian` Bloch Hamiltonian matrix $h(\mathbf k)$
- `fourierrules`, `blochphase`, `blochphaseexp`, `clusterhop`, `sumCluster`, `momentumbasis`

## States, Bases, and Matrices
- `qszbasisvc` basis with good charge $Q$ and spin projection $S_z$
- `qsbasisvc`, `qszbasis`, `qsbasis`, `quickISObasis`, `spinbasis`, `makebasis`, `basis`, `BASIS`, `mergebasis`, `transformbasis`, `applybasis`
- `makematricesbzvc` Hamiltonian matrices in the invariant subspaces
- `makematricesbzop`, `makeallmatricesbzvc`, `makeallmatricesbzop`, `matrixrepresentationvc`, `matrixrepresentationvcsparse`, `matrixrepresentationop`, `bzvc2bzop`, `bzop2bzvc`, `dropemptysubspaces`
- `vacuum`, `vc`, `ket`, `bra`, `ap`, `braketrule`, `vc2ops`, `op2ndx`, `ndx2op`, `scalarproductvc`, `normvc`, `expvvc`, `orthogvc`, `decomposevc`, `zeroonvac`
- `transformQStoIS`, `transformtoLR`, `transformtoPH`, `lrmap`
- `phononbasis`, `phononnumber`, `phononplus`, `phononminus`, `phononx`, `phononid`

## Expectation Values and Wick's Theorem
- `vev` vacuum expectation value
- `vevwick`, `wick`, `normalorderwick`, `contraction`, `dd`

## Quadratic Hamiltonians and Mean-Field Theory
- `quadraticform` BdG matrix of a quadratic Hamiltonian
- `bogoliubov` Bogoliubov transformation for fermions and bosons
- `bdghamiltonian`, `quadraticoperator`, `bogoliubovrules`, `gaussianaverage`
- `meanfield` Hartree, Fock, and pairing decoupling
- `meanfieldsolve`, `avg`

## Effective Hamiltonians
- `schriefferwolff` Schrieffer-Wolff transformation to arbitrary order
- `eigenoperators`, `schriefferwolffmatrix`

## Equations of Motion and Green's Functions
- `greensfunction` Green's function from closed equations of motion
- `liouvillian`, `spectralmoments`, `eomclosure`, `thermalaverage`, `lehmann`

## Output and Conversion
- `snegtex` LaTeX form of operator expressions
- `snegtoascii`, `asciitosneg`, `snegold2newrules`

## Extending SNEG
- `operatorQ`, `fermionQ`, `bosonQ`, `majoranaQ`, `grassmanQ`, `isnumericQ`, `iscreation`, `isannihilation`
- `snegOrderedQ`, `acmt`, `cmt`, `$SnegVersion`

## Tech Notes
- [First calculation: the Hubbard atom](tutorial/FirstCalculation)
- [Interfaces used by existing models](tutorial/CompatibilityInterfaces)

## Related Links
- [SNEG on GitHub](https://github.com/rokzitko/sneg)
- [NRG Ljubljana](https://github.com/rokzitko/nrgljubljana)
- [SNEG paper: Comput. Phys. Commun. 182, 2259 (2011)](https://doi.org/10.1016/j.cpc.2011.05.013)
