With four spins $1/2$ and equal antiferromagnetic bonds, a ring has five magnetization sectors. Diagonalize the blocks at $J=1$ and tabulate their dimensions and lowest energies:
```wl
snegspinoperators[{S,1/2}];
h = Sum[spinspin[S[i],S[Mod[i,4]+1]], {i,4}];
mm = spinclustermatrices[h,{S -> Range[4]}];
rows = {#[[1,1]],Length[Last[#]],Min[Eigenvalues[Last[#]]]} & /@ mm;
TableForm[rows,TableHeadings -> {None,{"Sz","Dimension","Energy / J"}}]
```
The minimum $-2J$ occurs only at $S^z=0$; a higher-spin ground multiplet would also have a representative with nonzero magnetization. The saturated states provide a sign check: each of four parallel bonds has energy $J/4$.
