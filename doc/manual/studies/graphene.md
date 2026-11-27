Each A orbital has B neighbours in cells $R$, $R-a_1$, and $R-a_2$. Extract the structure factor $-h_{AB}/t$; the other off-diagonal entry is its conjugate:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns]; snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1, a2];
hg = -t sum[hop[f[R,"A"], f[R,"B"]] + hop[f[R,"A"], f[R-a1,"B"]] +
  hop[f[R,"A"], f[R-a2,"B"]], {R}];
h = blochhamiltonian[fourier[hg,f -> fk,R -> k], {fk["A"],fk["B"]}, k];
Expand[-h[[1,2]]/t]
```
Evaluate the spectrum at the zone centre, a valley and an edge midpoint, in units of $t>0$. The bands touch at the valley. The Bravais lattice constant is one:
```wl
bands[p_] := Sort[Eigenvalues[Simplify[blochphaseexp[h,
  {k -> p, a1 -> {1,0}, a2 -> {1/2,Sqrt[3]/2}}] /. t -> 1]]];
TableForm[bands /@ {{0,0},{4 Pi/3,0},{0,2 Pi/Sqrt[3]}},
 TableHeadings -> {{"Centre","Valley","Midpoint"},{"Lower / t","Upper / t"}}]
```
