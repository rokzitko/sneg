---
name: CompatibilityInterfaces
keywords: legacy functions, developer interface, compatibility, NRG Ljubljana, declarations
---
# Interfaces used by existing models

The names below occur in older SNEG notebooks, model initializers or user-defined algebras. Some are stages in a larger calculation, with stronger preconditions than its public entry point. The alternatives given here preserve the stated convention; in particular a basis of creation strings and a basis of occupation vectors are not interchangeable.

## Operators and conventions
- `Hubbard` selects the open-chain Hubbard model in `hamiltonian`. It is a model identifier, distinct from the interaction constructor `hubbard`.
- `SPININDEXx`, `SPININDEXy`, `SPININDEXz`, `SPININDEXp`, `SPININDEXm` denote component indices 1 through 5 of a spin operator. `antisym[i,j]` encodes the signed remaining Cartesian index; `levicivita` is the tensor form.
- `spinflip` changes the last spin bit to one minus the bit. It applies to a single spin-half fermion operator, not to a many-body spin rotation. `outofdiag20` creates a local pair and `outofdiag02` annihilates one; compare the factor order before replacing them with `isospinplus` or `isospinminus`. `isospinx1` and `isospinx2` are the separate pair components of the transverse isospin.
- `su3g[i]`, for integer *i* from 1 to 8, gives the Gell-Mann matrices. For generators $t_a$ normalized by $\mathrm{Tr}(t_a t_b)=\delta_{ab}/2$, use `su3t[i]`, which divides the matrix by two.
- `xstates[X]` is the ordered state list of a declared Hubbard-operator family; `xparities[X]` maps state labels to parities. `snegxparity` gives the parity of the transition, the sum of the two state parities modulo two. Make these assignments through `snegxoperators`.
- `snegaddop` is the single-family fermion registration step. Use `snegfermionoperators` or `snegspinlessfermionoperators`. `snegnonopQ` marks commuting symbols or function heads. A Grassmann coefficient requires its own declaration, not this marker.
- `snegpositiveconstants` also installs rules on `Power` for square roots. For local simplifications use a real-constant declaration and explicit positive assumptions instead.

## Symbolic sums and simplification
- `snegsumJoinWithRenaming` joins iterator lists when multiplying symbolic sums. `ssJWRnewname` creates and declares an index avoiding the names already in use; `FreeOfIndexQ` tests independence from all indices in a list. Use `nc` on the two sums to include capture avoidance.
- `der` supplies derivative rules for combinations containing formal sums. Specify which quantities, including the range and the reference state, are fixed when taking a physical derivative.
- `fastpow[op,n]` forms a noncommutative power with intermediate simplifications. `pow` is the direct power constructor. `ruleAbs` replaces a scalar times its conjugate by a squared magnitude.
- `SnegSimplifySpin` recognizes spin combinations; `SnegFullSimplify` uses several SNEG transformations within `Simplify`. As with `SnegSimplify`, a recognized constructor can be held for display. Release it before comparing expanded expressions.
- `maskOp` evaluates an operation with declared constants temporarily replaced, then restores their names. The temporary symbols do not carry the physical assumptions of the originals.

## Contractions and vacuum reduction
- `indecesCR` and `indecesAN` locate the creation and annihilation slots of a string; the spelling is historical. `maxcran` bounds the number of contractions. `contract` evaluates a specified list of paired positions, `remainder` gathers the uncontracted operators, and `wickorder` selects terms with a prescribed number of contractions. `wick` includes all orders.
- `vevwick2` and `vevwicknew` are alternative evaluation paths with early tests that discard terms; `vevwick` is the general entry point. `Problem` is a diagnostic head in the former path, not an expectation value.
- `Zeroonvac` extends `zeroonvac` to additional scalar and formal-sum cases. In the creation-string representation `applytocr[op,state]` also normalizes its nonzero result; it should not be used to measure a transition amplitude.

## Symmetry sectors and matrices
- `ADD` is a recursive addition step in building a spin multiplet basis. Use `qsbasis` for a complete set of levels. `SU2combine` couples individual highest-weight states with Clebsch-Gordan coefficients; `SU2merge` assembles the resulting sectors. `qscombinefnc` adds the subsystem charges and `qsspincombinefnc` retains the charge of the fermionic subsystem when attaching a spin. Use `SU2basistensorproduct` for the full product.
- `Tminus` is the global lowering operator used by the one-argument `transformQStoIS`. Supplying the operator explicitly avoids dependence on an earlier model. `transform1` is one step in removing the descendants from lower-charge sectors.
- `mambpair` calculates a transition block for a pair of sectors. Use `makeallmatricesbzvc` or `makeallmatricesbzop` for all connected pairs. `indexvalue2list` places indexed coefficients into a vector; the ordered basis fixes the meaning of the index.
- `snegorthog` orthogonalizes coefficient vectors with a conjugated scalar product. For operator strings use `orthogop` and for occupation vectors `orthogvc`. `pairpattern` tests compatibility of bra and ket slots, including placeholders.
- `ZeroMatrix[n]` constructs a square zero matrix. `ConstantArray[0,{n,n}]` is an equivalent dense array.

## Utilities and output
- `addto` adds an element to a list and removes duplicates. `allcombinations` forms a product of lists of alternatives (`Tuples`); `KS` builds index subsets for the combinatorial routines. `listofpairs` forms ordered pairs drawn from two lists, and `dropzeros` removes literal zeros at the top level.
- `TeXString` converts through `TeXForm`; `SnegTeXForm` installs historical formatting rules. Use `snegtex` for a LaTeX string in SNEG operator notation. `UsageWithMore` assigns a usage message; its old browser-target argument is ignored.
- `snegidstring` and `snegcopyright` supply startup text. Read `$SnegVersion` for the package version. `free` is reserved in the public context for compatibility and is not an operator constructor.

## Related Guides
- [SNEG](guide/SNEG)
