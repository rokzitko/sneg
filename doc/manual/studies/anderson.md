For a particle-hole symmetric dot and a single bath level at zero energy the half-filled singlet hybridizes with two virtual charge states. Tabulate the energies at $U=4$, $V=1/2$ using one row per multiplet. Here $Q=N-2$ and the energy zero is the empty system:
```wl
snegfermionoperators[d,c]; snegrealconstants[U,V];
h = -U number[d[]]/2 + U hubbard[d[]] + V hop[d[],c[]];
b = Select[qsbasisvc[{d[],c[]}], #[[1,1]] == 0 &];
m = makematricesbzvc[h,b];
rows = Flatten[Table[{blockdata[[1,2]],#} & /@
  Sort[Eigenvalues[Last[blockdata] /. {U -> 4,V -> 1/2}]],
  {blockdata,m}],1];
TableForm[rows,TableHeadings -> {None,{"Total spin","Energy"}}]
```
The isolated triplet has energy $-U/2$. The lower singlet is $-(U+\sqrt{U^2+64 V^2})/4$. Check the leading difference against an antiferromagnetic exchange rather than using the entire singlet energy as the exchange:
```wl
gap = -U/2 + (U+Sqrt[U^2+64 V^2])/4;
Limit[gap/V^2, V -> 0, Assumptions -> U > 0]
```
