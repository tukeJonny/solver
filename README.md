# solver

tiny sat solver written in Haskell


## Run

If we found satisfiable assignments, solver prints `SATISFIABLE` .

```
$ cat ./examples/test1.cnf
p cnf 5 6
1 2 0
-3 -1 0
-4 -3 0
2 -5 0
5 -2 0
1 4 0

$ # Do not forget cnf file's newline at EOF
$ stack run ./examples/test1.cnf
s SATISFIABLE
v 1 2 -3 5 0
```

If we can't find out satisfiable assignments, solver prints `UNSATISFIABLE` .

```
$ cat ./examples/test2.cnf
p cnf 2 4
1 2 0
1 -2 0
-1 2 0
-1 -2 0

$ stack run ./examples/test2.cnf
s UNSATISFIABLE
```

If given number of literals is wrong, solver prints NumLitsError

```
$ stack run ./examples/test4-invalid.cnf
solver-exe: NumLitsError {gotLits = 2}
```

If given number of clauses is wrong, solver prints NumClausesError

```
$ stack run ./examples/test5-invalid.cnf
solver-exe: NumClausesError {gotClauses = 4}
```


## Test

There are serveal hspec tests.

```
$ stack test
solver> test (suite: solver-test)


Clause
  hasLit
    included
    not included
  excludeInvLit
    can exclude
    can not exclude
  collectPureLits
    can filter
    can not filter
DPLL
  addAssignment
    assign
  isSat
    nothing
    assignment
  isSingleLitClause
    single lit clauses
    multiple lit clauses
  assignToSingleLits
    satisfiable
    unsatisfiable
  dpll
    satisfiable
    unsatisfiable
Literal
  invert
    do invert
    get same value by inverting twice
  isOpposite
    same
    opposite
  getTrue&getFalse
    getTrue
    getFalse

Finished in 0.0019 seconds
21 examples, 0 failures

solver> Test suite solver-test passed
```