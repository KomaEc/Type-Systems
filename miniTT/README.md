# miniTT

## Parser
Ambiguities and how to resolve in Happy:

* `Π p : exp1 . 1 . exp2`. The first dot should be reduced rather than shifted. The allowed expression should be `Π p : (exp1 . 1) . exp2`. To resolve, add context Nonterminal `ColonExprDot` to indicate the case mentioned above.s