---
keywords: version, release, package version
see also: $Version
---
# $SnegVersion

## Usage
- `$SnegVersion` is a string with the version number of the loaded SNEG package.

## Details
- The version consists of the major, minor, and patch numbers, e.g. `"2.2.0"`. It is also printed when SNEG is loaded.
- When reporting results, record `$SnegVersion` together with `$Version`.

## Examples
### Basic Examples
```wl
$SnegVersion
```
Test for a required minimal version, here 2.0.0:
```wl
OrderedQ[{{2, 0, 0}, ToExpression[StringSplit[$SnegVersion, "."]]}]
```
