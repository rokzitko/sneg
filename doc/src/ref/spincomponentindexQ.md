---
keywords: spin component index, predicate
see also: snegspincomponentindexes, listspincomponentindexes, levicivita
---
# spincomponentindexQ

## Usage
- `spincomponentindexQ[a]` gives `True` if *a* has been declared as a symbolic spin component index with `snegspincomponentindexes`, and `False` otherwise.

## Details
- The explicit component values 1, ..., 5 are not spin component indexes in this sense.
- The declaration sets the upvalue `spincomponentindexQ[a] ^= True`.

## Examples
### Basic Examples
```wl
snegspincomponentindexes[a];
{spincomponentindexQ[a], spincomponentindexQ[b], spincomponentindexQ[3]}
```
