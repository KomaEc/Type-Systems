# Changelog for miniTT

## Unreleased changes
1. Modifying the syntax in the original paper : a program consists of a list of declaration, not a series of declaration and an expression. Reason : it's not convenient to write a parser for the original syntax, for a variable 'x', parser cannot detect whether it should be the start of a declaration (a pattern) or an expression. However, the internal language is kept as the same.

2. The first letter of constructor must be in uppercase, and that of variable must be in lowercase.
