Follow just the emission of a phonon by an up electron. The absorption term is its Hermitian conjugate and the other spin is a second copy. This isolates the momentum transfer without expanding four terms related by symmetry:
```wl
snegfermionoperators[c, ck]; snegbosonoperators[b, bk];
snegrealconstants[g]; sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k,q}, Ns];
emit = g sum[nc[number[c[R],UP], b[CR,R]], {R}];
emk = fourier[fourier[emit,c -> ck,R -> k], b -> bk,R -> q];
sumMomentumCanonical[sumExpand[emk]]
```
The incoming electron momentum exceeds the outgoing one by $q$. The site sum has removed one independent momentum and left the local coupling divided by $\sqrt{N_s}$.
