---
name: FirstCalculation
keywords: getting started, Hubbard atom, double occupancy, spin multiplet, eigenvalues
---
# First calculation: the Hubbard atom

What does a repulsion cost when there is only one electron on a level? Work with one spin-degenerate orbital, build its Hamiltonian, and compare the sectors before substituting any numerical parameters.

## Declare the operators
```wl
Needs["sneg`"];
snegfermionoperators[d];
snegrealconstants[\[Epsilon],U];
```
`d[]` denotes the level; `d[AN,UP]` removes its up electron. The anticommutator is one on the same spin orbital and zero between different spin orbitals:
```wl
Table[antikomutator[d[AN,s],d[CR,r]], {s,{UP,DO}}, {r,{UP,DO}}] // MatrixForm
```

## Construct and read the blocks
The Hamiltonian is $H=\epsilon n+U n_\uparrow n_\downarrow$. Use `nc` for products of operators; ordinary multiplication here attaches real coefficients to already constructed operators.
```wl
h = \[Epsilon] number[d[]] + U hubbard[d[]];
m = makematricesbzvc[h,qsbasisvc[{d[]}]];
TableForm[{Sequence @@ First[#], #[[2,1,1]]} & /@ m,
 TableHeadings -> {None,{"Q","Spin","Energy"}}]
```
Here $Q=N-1$. The middle row represents two spin projections. It has no interaction contribution: adding the first electron costs $\epsilon$, adding a second costs $\epsilon+U$. Multiplying every row by its spin multiplicity recovers the four Fock states:
```wl
Total[(2 #[[1,2]]+1) Length[Last[#]] & /@ m]
```

## A check in the occupation basis
At $\epsilon=-U/2$ and positive $U$ the two spin states are the ground states. Their equal mixture has no mean spin but a finite local moment:
```wl
observables = {number[d[]],spinz[d[]],pow[spinz[d[]],2]};
TableForm[{thermalaverage[#,h /. {\[Epsilon] -> -1,U -> 2},{d},0]} & /@ observables,
 TableHeadings -> {{"Occupation","Spin z","Squared spin z"},{"Mean"}}]
```

## Related Guides
- [SNEG](guide/SNEG)
