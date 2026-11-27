"""Typeset the canonical help-page usage, details, options and first example."""
from pathlib import Path
import json
import re

here = Path(__file__).resolve().parent
src = here.parent / "src"
out = here / "generated"
out.mkdir(exist_ok=True)


def esc(s):
    specials = dict(zip("#$%&_{}", (r"\#", r"\$", r"\%", r"\&", r"\_", r"\{", r"\}")))
    specials.update({"\\": r"\textbackslash{}", "^": r"\textasciicircum{}",
                     "~": r"\textasciitilde{}"})
    return "".join(specials.get(c, c) for c in s)


def inline(s):
    # Handle code first: $SnegVersion and Mathematica patterns are literals.
    tokens = re.split(r"(`[^`]*`|\$[^$\n]+\$|\*\*[^*]+\*\*|\*[A-Za-z][\w -]*\*)", s)
    result = []
    for i, token in enumerate(tokens):
        if i % 2 == 0:
            result.append(esc(token))
        elif token.startswith("`"):
            literal = re.sub(r"\beps\b", "ε", token[1:-1])
            result.append(r"\texttt{" + re.sub(r"([\[\],])", r"\1\\allowbreak{}", esc(literal)).replace("∈", r"\ensuremath{\in}") + "}")
        elif token.startswith("$"):
            result.append(token)
        else:
            cmd = r"\textbf" if token.startswith("**") else r"\emph"
            result.append(cmd + "{" + esc(token.strip("*")) + "}")
    return "".join(result)


def paragraphs(text):
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("##"):
            continue
        if line.startswith("|"):
            cells = [c.strip() for c in re.split(r"\s+\|\s+", line.strip("| "))]
            if not re.fullmatch(r"[\s|:-]+", line):
                yield r"\par\noindent " + r"\enspace ".join(map(inline, cells))
        elif line.startswith("$$") and line.endswith("$$"):
            yield r"\refdisplay{" + line[2:-2] + "}"
        else:
            yield (r"\par " if line.startswith("- ") else "") + inline(line.removeprefix("- "))


pages = {}
file_ids = {}
for file in (src / "ref").glob("*.md"):
    text = file.read_text()
    name = re.search(r"^# (.+)$", text, re.M)[1]
    pages[name] = text
    file_ids[name] = file.stem

guide = (src / "guide/SNEG.md").read_text()
groups = {}
assigned = {}
for title, content in re.findall(r"^## (.+)\n([\s\S]*?)(?=^## |\Z)", guide, re.M):
    if title not in {"Related Links", "Tech Notes", "Related Guides"}:
        groups[title] = []
        for name in re.findall(r"`([^`]+)`", content):
            if name in pages:
                assigned.setdefault(name, title)
for name, page in pages.items():
    if name not in assigned:
        links = re.search(r"^see also: (.*)$", page, re.M)
        nearby = links[1].split(", ") if links else []
        groups.setdefault("Additional interfaces", [])
        group = next((assigned[n] for n in nearby if n in assigned), "Additional interfaces")
    else:
        group = assigned[name]
    groups[group].append(name)

lines = [r"\begingroup\small\raggedright"]
index = []
first_inputs = {}
labs = out / "labs"
labs.mkdir(exist_ok=True)
# For these short algebraic results a printed output is more informative
# than the input alone. Longer examples are worked in Parts I and II.
print_result = set("""number hubbard hop conj nc pow komutator antikomutator
phononx PauliY PauliZ spinmatrixZ bcs spinplus spinminus spinz projector1
thermalaverage greensfunction""".split())
for group, names in groups.items():
    if not names:
        continue
    lines.append(r"\section{" + esc(group) + "}")
    for name in sorted(names, key=str.casefold):
        page = pages[name]
        label = "ref:" + name.replace("$", "dollar-")
        lines.append(r"\subsection*{\texttt{" + esc(name) + r"}}\phantomsection\label{" + label + "}")
        index.append((name, label))
        body = re.split(r"^## (?:Developer Notes|Examples)\s*$", page, flags=re.M)[0].split("## Usage", 1)[1]
        lines.extend(paragraphs(body))
        example = re.search(r"^### Basic Examples\s*\n(.*?)^```wl\n(.*?)^```", page, re.M | re.S)
        if example:
            code = example[2].rstrip()
            first_inputs[file_ids[name]] = code
            lines.append(r"\par\smallskip\noindent\textit{Example.} " + inline(example[1].strip()))
            if name in print_result:
                key = "lab-ref-" + name
                (labs / (key + ".tex")).write_text(
                    "\\setwlchapter{" + key + "}\n\\begin{wl}\n" +
                    re.sub(r"\beps\b", r"\\[Epsilon]", code) + "\n\\end{wl}\n")
                lines.append(r"\input{generated/labs/" + key + "}")
            else:
                lines.extend([r"\begin{lstlisting}[style=wl,language=SNEG]",
                              re.sub(r"\beps\b", "ε", code).replace(r"\[Epsilon]", "ε").replace("ε", r'(*@\char"03B5{}@*)'),
                              r"\end{lstlisting}"])
lines.append(r"\endgroup")
(out / "reference.tex").write_text("\n".join(lines) + "\n")
(out / "reference-inputs.json").write_text(json.dumps(first_inputs))
compat = (src / "tutorial/CompatibilityInterfaces.md").read_text().split("## Related Guides", 1)[0]
legacy_sections = []
for i, (title, text) in enumerate(re.findall(r"^## (.+)\n([\s\S]*?)(?=^## |\Z)", compat, re.M)):
    label = f"compat:{i}"
    legacy_sections.append(r"\subsection{" + esc(title) + r"}\label{" + label + "}")
    legacy_sections.extend(paragraphs(text))
    for symbol in re.findall(r"`([A-Za-z$][A-Za-z0-9$]*)(?:\[.*?)?`", text):
        if symbol not in pages and symbol not in {n for n, _ in index} and symbol not in {"Tuples", "Power", "Simplify", "TeXForm", "ConstantArray"}:
            index.append((symbol, label))
(out / "compatibility.tex").write_text("\\begingroup\\raggedright\n" + "\n".join(legacy_sections) + "\n\\endgroup\n")
(out / "symbol-index.tex").write_text(
    "\\begin{multicols}{2}\\raggedright\\small\n" +
    "\n".join(r"\noindent\hyperref[" + label + r"]{\texttt{" + esc(name) +
              r"}}\dotfill\pageref{" + label + r"}\par" for name, label in sorted(index, key=lambda x: x[0].casefold())) +
    "\n\\end{multicols}\n")
print(f"Typeset {len(pages)} reference entries.")

# Each case study is run in a separate kernel, as in the help page.
# Match a distinctive phrase in its introduction, so inserting a new
# example in the help does not silently change the one printed here.
study_text = {}
for line in (here / "case-studies.tsv").read_text().splitlines():
    if not line or line.startswith("#"):
        continue
    key, *selection = line.split("\t")
    if selection:
        name, section, match = selection
        part = re.search(r"^### " + re.escape(section) + r"\s*\n(.*?)(?=^### |\Z)", pages[name], re.M | re.S)[1]
        groups = [g.strip() for g in re.split(r"^---\s*\n|(?=^#### )", part, flags=re.M) if g.strip()]
        found = [g for g in groups if match.casefold() in g.split("```", 1)[0].casefold()]
        if len(found) != 1:
            raise ValueError(f"{key}: expected one {name} example matching {match!r}, got {len(found)}")
        group = found[0]
    else:
        group = (here / "studies" / (key + ".md")).read_text()
    chunks = re.split(r"^```wl\n(.*?)^```", group, flags=re.M | re.S)
    lines = [r"\setwlchapter{lab-" + key + "}"]
    for i, chunk in enumerate(chunks):
        if i % 2:
            code = re.sub(r"\beps\b", r"\\[Epsilon]", chunk.rstrip())
            chunks[i] = code + "\n"
            lines += [r"\begin{wl}", code, r"\end{wl}"]
        else:
            chunk = re.sub(r"^#### [^\n]+\n?", "", chunk.strip())
            lines.extend(paragraphs(chunk.strip()))
    (labs / ("lab-" + key + ".tex")).write_text("\n".join(lines) + "\n")
    study_text[key] = "".join("```wl\n" + text + "```" if i % 2 else text for i, text in enumerate(chunks))
(out / "studies.json").write_text(json.dumps(study_text, ensure_ascii=False))
