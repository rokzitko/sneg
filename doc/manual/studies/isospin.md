The half-filled bipartite Hubbard dimer has independent spin and charge SU(2) symmetries. Use the particle-hole symmetric interaction and opposite phases for the pair-lowering operators at the two ends of the bond. Check the commutator before reducing the basis:
```wl
snegfermionoperators[c];
snegrealconstants[t,U];
sites = {c[1],c[2]};
tm = isospinminus[c[1],0] + isospinminus[c[2],1];
h = -t hop[c[1],c[2]] + U Total[isozsq[sites]]/2;
Expand[komutator[h,tm]]
```
Each row below is one multiplet, not a single state. For example the $I=1$, $S=0$ row contains the vacuum, the fully occupied state and a half-filled singlet; all have the same energy with this energy origin:
```wl
bz = transformQStoIS[qsbasisvc[sites],tm];
mm = makematricesbzvc[h,bz];
rows = Join @@ Table[{Sequence @@ First[blockdata],#} & /@ Eigenvalues[Last[blockdata]], {blockdata,mm}];
TableForm[rows, TableHeadings -> {None,{"Isospin","Spin","Energy"}}]
```
Restoring the missing projections recovers all 16 Fock states:
```wl
Total[(2 #[[1]]+1)(2 #[[2]]+1) & /@ rows]
```
