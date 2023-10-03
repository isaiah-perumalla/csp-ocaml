# setup
1. `opam switch create 4.14.1 .
2. `eval $(opam env)`
3. `opam install core core_bench utop`


# CSP problem
this problem can be viewed as a constriant statifaction problem and can be solved using backtracking search

Goal was to see what it would look like to solve it in ocaml

1. Thinking in types and language support for this is great
2. Ocaml modules are unique , learn a lot about functors
3. having to work with immutable functional data structure makes backtracking search trivial


## Inputs output

```
XXXXXXXXXX
X      XXX
X X   XXXX
X     XXXX
X     XX X
XXXXXXXXXX

```
Constraints
```
- a,3,1
- b,2,2
- c,2,2
- d,2,5
```

output 

```
XXXXXXXXXX                         
X      XXX
X X   XXXX
X  aaaXXXX
XbcdbcXXdX
XXXXXXXXXX

```