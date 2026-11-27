<|"Source" -> "bosonQ.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegbosonoperators[a];\n{bosonQ[a], bosonQ[a[CR, 1]], \
TrueQ[bosonQ[u]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "False"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegfermionoperators[d];\nUnion[Cases\
[nc[a[CR], d[CR, UP], d[AN, UP], a[AN]], _?bosonQ, Infinity]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "}"}], "Out[3]="}}|>}|>
