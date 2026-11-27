# SNEG documentation sources

The help pages of SNEG (reference pages, guide pages, and tech notes) are
generated from Markdown sources in `doc/src`. The examples are evaluated
when the documentation is built, and their outputs are cached in
`doc/cache` and committed, so that the help pages can be rebuilt without
re-evaluating unchanged examples and changes of the outputs show up in
`git diff`.

```
doc/src/ref/<symbol>.md      reference page of a symbol
doc/src/guide/<name>.md      guide page (doc/src/guide/SNEG.md is the main page)
doc/src/tutorial/<name>.md   tech note
doc/cache/                   cached example outputs (generated, committed)
doc/tools/SnegDoc.wl         parser, TeX converter, notebook assembly
doc/tools/run-examples.wl    evaluates the examples of one page in a fresh kernel
doc/build-docs.wl            builds the help pages
doc/test-docs.m              checks the examples against the cache
```

## Building

```sh
wolframscript -file doc/build-docs.wl            # all pages
wolframscript -file doc/build-docs.wl nc hubbard # selected pages
wolframscript -file doc/build-docs.wl force nc   # re-evaluate the examples
wolframscript -file doc/test-docs.m              # verify all examples
wolframscript -file doc/test-help-dollars.wl     # check built help for stray math delimiters
```

The examples are evaluated in up to four parallel kernels; set the
environment variable `SNEGDOC_JOBS` to change this.

### LaTeX manual

The stand-alone tutorial and reference in `manual/sneg-manual.tex` builds from the repository
root with Mathematica, Python 3.9+, `latexmk`, LuaLaTeX and Biber (tested with TeX Live
2021). It uses KOMA-Script, TeX Gyre Pagella, Fira Sans and Fira Mono.

```sh
wolframscript -file doc/manual/build-manual.wl
wolframscript -file doc/manual/build-notebooks.wl
wolframscript -file doc/test-docs.m correlated-electrons correlated-solutions
```

Open `doc/manual/sneg-manual.pdf`. Each `chapters/<name>.tex` starts its own
kernel session and selects its listings with `\setwlchapter{<name>}`.
Put executable Wolfram Language between `\begin{wl}` and `\end{wl}`, each
on its own line. Unlike help-page example groups, successive manual
listings share their definitions and line numbers throughout the chapter.
The builder preserves the evaluated examples in `manual/cache`, generates
the input listings and typeset output in `manual/generated`, then runs
LaTeX and Biber. Use `force` to refresh the cache after an intentional
package change. `doc/test-docs.m` re-evaluates the chapter and solution
appendix in temporary kernels without rewriting their baselines.

`manual/reference.py` derives the grouped reference and symbol index from
the help sources. Each named case study in `manual/case-studies.tsv` selects
a particular help example or a source in `manual/studies/`. Each study runs
in an independent kernel. The notebook exporter uses the same source and
cached outputs and refuses stale code. The files in `doc/notebooks/` open
in Mathematica and are intended for distribution from auger.ijs.si.
To check a downloaded notebook, run
`wolframscript -file doc/manual/test-notebook.wl doc/notebooks/hubbard-dimer.nb`;
this evaluates the notebook's input boxes in order and compares its output cells.
The manual, help text and teaching notebooks are licensed under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

The help pages for `BASIS` and `VACUUM` use file IDs `BasisVariable` and
`VacuumKet`. These coexist with `basis` and `vacuum` on case-insensitive
filesystems. Use the file ID to select one of these pages for a build or test.

`build-docs.wl` evaluates the examples of pages whose cache is missing or
whose code changed, writes documentation source notebooks in the format of
the Wolfram DocumentationTools to `build/docsrc/SNEG`, builds the help pages
with `PacletDocumentationBuild` into `build/docbuild`, and copies them to
`Documentation/` in the repository (not committed). A checkout used as a
paclet (with `PacletDirectoryLoad` or a link in
`$UserBaseDirectory/Applications`) then has working help pages.
`build-paclet.m` generates the source notebooks and builds the release
paclet, including the help pages.

`test-docs.m` re-evaluates the examples of all pages in fresh kernels and
compares every output, printed output, and message with the cache. Run it
after changes of the package; if an output changed intentionally, update the
cache with `build-docs.wl force <page>` and review `git diff doc/cache`.

Each page is evaluated in its own kernel, with `sneg.m` from the repository
loaded, `$Context = "Global`"`, and a fixed random seed. Operator
declarations therefore do not carry over between pages, but they do carry
over between the examples of one page. Each example group should
nevertheless declare the operators it uses, because readers evaluate the
examples individually.

## Reference pages

~~~markdown
---
keywords: comma-separated search keywords
see also: komutator, antikomutator, conj
tech notes: [Title](tutorial/Name); [Other title](tutorial/Other)
guides: [SNEG](guide/SNEG)
related links: [Paper](https://doi.org/...); [Other](https://...)
---
# nc

## Usage
- `nc[a, b, c, ...]` represents the noncommutative product ...
- `nc[]` is 1, ...

## Details
- One bullet per note. Text can contain `code`, $TeX$ formulas,
  *arguments*, **bold** text, and [links](ref/komutator).

| `"Option"` | `default` | description |

## Developer Notes
- Implementation details, extension points, and internal conventions.

## Examples
### Basic Examples
Text before an example.
```wl
snegfermionoperators[c];
nc[c[CR, 1, UP], c[AN, 2, UP]]
```
---
(a delimiter between independent examples)

### Scope
#### A subsection
...
~~~

- The title is the symbol name. All sections except Usage and Basic
  Examples are optional. The example sections are, in this order: Basic
  Examples, Scope, Generalizations & Extensions, Options, Applications,
  Properties & Relations, Possible Issues, Interactive Examples, and Neat
  Examples.
- Usage lines start with the template in backticks. In code, identifiers
  that are SNEG symbols with a reference page or System symbols become
  links; other SNEG symbols are set upright, and all other identifiers in
  italics as arguments.
- Options tables have two or three columns; the last column is the
  description.
- Developer notes appear at the end of the details section.
- A code block is one input cell. As in a notebook, every top-level
  expression gives an output unless it ends with `;`, and `%` refers to the
  previous output. Messages are shown as in a notebook; use them only to
  document error handling (Possible Issues).

## Formulas

Formulas are written in TeX between `$...$` (inline) or `$$...$$` (separate
paragraph) and converted to TraditionalForm boxes. The supported subset is
the one needed for physics notation: sub- and superscripts, `\frac`,
`\sqrt`, `\sum` and `\prod` with limits, Greek letters, `\dagger`,
`\uparrow`, `\downarrow`, `\langle`, `\rangle`, `\cdot`, relations and
arrows, `\mathrm`, `\text`, `\mathbf`, `\boldsymbol`, `\mathcal`, accents
(`\hat`, `\tilde`, `\bar`, `\dot`, `\vec`), spacing commands, and the
standard function names (`\cos`, `\exp`, `\ln`, ...). Unsupported commands
stop the build with an error. As in TeX, adjacent letters are set without
space: `\delta_{ij}`.

## Guide pages

~~~markdown
---
name: SNEG
keywords: ...
---
# Page title

Abstract paragraphs.

## Function group
- `snegfermionoperators` short description
- `snegbosonoperators`, `snegspinoperators`, `snegmajoranaoperators`

## Related Links
- [SNEG on GitHub](https://github.com/rokzitko/sneg)
~~~

A bullet with a single function and a description gives a line of the
function list; a bullet with only functions gives an inline listing. The
sections Tech Notes, Related Guides, and Related Links contain links.

## Tech notes

Tech notes have `name:` in the front matter, `##` sections, `###`
subsections, text, formulas, tables, and evaluated code blocks.
