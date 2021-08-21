# lexproj
Compiler experiments

## JSON Free Context Grammar

### J -> A
### A -> [C] | {K} | {} | []
### C -> TEC | T
### K -> "key":TEK | "key":T
### E -> ,
### T -> "str" | 0-9 | 0.0-9.9 | A | true, false
