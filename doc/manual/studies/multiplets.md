Three spinful orbitals have 64 many-body states. List the number of independent highest-weight states at each charge and spin; the last column restores the omitted spin projections:
```wl
snegfermionoperators[c];
b = qsbasisvc[c /@ Range[3]];
dims = {Sequence @@ First[#], Length[Last[#]], (2 #[[1,2]]+1) Length[Last[#]]} & /@ b;
TableForm[dims, TableHeadings -> {None,{"Q","S","Multiplets","States"}}]
```
The sum rule counts copies of each representation as well as its dimension:
```wl
Total[dims[[All,-1]]] == 4^3
```
