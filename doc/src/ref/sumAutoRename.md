---
keywords: dummy index renaming, index collision, product of sums, symbolic sum
see also: sum, nc, komutator, snegfreeindexes
---
# sumAutoRename

## Usage
- `sumAutoRename` is a global variable that controls the automatic renaming of dummy indexes in products of symbolic sums. The default is `True`.

## Details
- With `sumAutoRename = True`, a product `nc[sum[a, {k}], sum[b, {k}]]` is combined into one sum in which the second dummy index is renamed to `k1` (or `k2`, ...), the first name that is not among the indexes of the two sums and does not occur in the expression. The new index is declared with `snegfreeindexes` and inherits the properties of `k` (realness, lattice or momentum type, spin-component type). `komutator` of two sums renames the indexes of the second sum in both products.
- With `sumAutoRename = True`, nested sums inside a sum of terms, `sum[sum[a, {i}] + c, {j}]`, are kept nested so that the renaming rules see the full expression; use `sumExpand` to distribute.
- With `sumAutoRename = False`, the index lists are joined without renaming. If they overlap, the message `snegsumJoin::overlap` is issued and the result contains the same index twice, which is wrong; nested sums inside a sum of terms are distributed automatically.
- Use `Block[{sumAutoRename = False}, ...]` to change the setting temporarily.

## Examples
### Basic Examples
```wl
sumAutoRename
```
```wl
snegfermionoperators[c];
snegfreeindexes[k];
nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, UP], {k}]]
```

### Scope
Without renaming, sums with different indexes are still combined:
```wl
snegfermionoperators[c];
snegfreeindexes[k, l];
Block[{sumAutoRename = False}, nc[sum[c[CR, k, UP], {k}], sum[c[AN, l, UP], {l}]]]
```
---
Nested sums inside a sum of terms:
```wl
snegfreeindexes[k, l];
snegrealfunctions[g];
snegrealconstants[x];
{sum[sum[g[k, l], {k}] + x, {l}], Block[{sumAutoRename = False}, sum[sum[g[k, l], {k}] + x, {l}]]}
```

### Possible Issues
Without renaming, a collision of dummy indexes gives a wrong result:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
Block[{sumAutoRename = False}, nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, UP], {k}]]]
```
