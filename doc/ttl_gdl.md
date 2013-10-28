## Tic Tac Toe GDL Example (from Stanford)
### Explanation of base relations
    role(a) means that a is a role in the game.
    base(p) means that p is a base proposition in the game.
    input(r,a) means that a is an action for role r.
    init(p) means that the proposition p is true in the initial state.
    true(p) means that the proposition p is true in the current state.
    does(r,a) means that player r performs action a in the current state.
    next(p) means that the proposition p is true in the next state.
    legal(r,a) means it is legal for role r to play action a in the current state.
    goal(r,n) means that player the current state has utility n for player r.
    terminal means that the current state is a terminal state.

### Roles
    role(x)
    role(o)

### Feasible actions and inputs
    input(R,mark(M,N)) :- role(R) & index(M) & index(N)
    input(R,noop) :- role(R)

    index(1)
    index(2)
    index(3)

### Input and state -related propositions
    base(cell(M,N,x)) :- index(M) & index(N)
    base(cell(M,N,o)) :- index(M) & index(N)
    base(cell(M,N,b)) :- index(M) & index(N)

    base(control(x))
    base(control(o))

### Initial state
    init(cell(1,1,b))
    init(cell(1,2,b))
    init(cell(1,3,b))
    init(cell(2,1,b))
    init(cell(2,2,b))
    init(cell(2,3,b))
    init(cell(3,1,b))
    init(cell(3,2,b))
    init(cell(3,3,b))
    init(control(x))

### Legality
    legal(W,mark(X,Y)) :-
      true(cell(X,Y,b)) &
      true(control(W))

    legal(x,noop) :-
      true(control(o))

    legal(o,noop) :-
      true(control(x))

### Update rules
    next(cell(M,N,R)) :-
      does(R,mark(M,N)) &
      true(cell(M,N,b))

    next(cell(M,N,W)) :-
      true(cell(M,N,W)) &
      distinct(W,b)

    next(cell(M,N,b)) :-
      does(W,mark(J,K))
      true(cell(M,N,W)) &
      distinct(M,J)

    next(cell(M,N,b)) :-
      does(W,mark(J,K))
      true(cell(M,N,W)) &
      distinct(N,K)

    next(control(x)) :-
      true(control(o))

    next(control(o)) :-
      true(control(x))

### Goals and scoring
    goal(x,100) :- line(x) & ~line(o)
    goal(x,50) :- ~line(x) & ~line(o)
    goal(x,0) :- ~line(x) & line(o)

    goal(o,100) :- ~line(x) & line(o)
    goal(o,50) :- ~line(x) & ~line(o)
    goal(o,0) :- line(x) & ~line(o)

### Supporting concepts and helpers
    line(X) :- row(M,X)
    line(X) :- column(M,X)
    line(X) :- diagonal(X)

    row(M,X) :-
      true(cell(M,1,X)) &
      true(cell(M,2,X)) &
      true(cell(M,3,X))

    column(M,X) :-
      true(cell(1,N,X)) &
      true(cell(2,N,X)) &
      true(cell(3,N,X))

    diagonal(X) :-
      true(cell(1,1,X)) &
      true(cell(2,2,X)) &
      true(cell(3,3,X)) ;;& TYPO?

    diagonal(X) :-
      true(cell(1,3,X)) &
      true(cell(2,2,X)) &
      true(cell(3,1,X)) ;;& TYPO?

### Termination
    terminal :- line(W)
    terminal :- ~open

    open :- true(cell(M,N,b))

## Observations

### And and ors and as they relate to core.logic
Here's an example:

    diagonal(X) :-
      true(cell(1,1,X)) &
      true(cell(2,2,X)) &
      true(cell(3,3,X)) &

    diagonal(X) :-
      true(cell(1,3,X)) &
      true(cell(2,2,X)) &
      true(cell(3,1,X)) &

Assuming that last & is a typo...

    diagonal(X) :-
      (true(cell(1,1,X)) &
       true(cell(2,2,X)) &
       true(cell(3,3,X))) | 
      (true(cell(1,3,X)) &
       true(cell(2,2,X)) &
       true(cell(3,1,X)))

In core.logic, multiple goals listed is the equivalent of an AND:

```Clojure
(fresh [x y] 
  (== x 1) 
  (== y 2))
```

--> Succeeds if x == 1 AND y == 2

And a conde is the equivalent of an OR:

```Clojure
(fresh [x y]
  (conde
    [(== x 1)]
    [(== y 2)]))
```

--> Succeeds if x == 1 OR y == 2

This can be used to translate diagonal to core.logic easily:

```Clojure
(defn diagonalo [x]
  (conde
    [(trueo [:cell [1 1 x]])
     (trueo [:cell [2 2 x]])
     (trueo [:cell [3 3 x]])]
    [(trueo [:cell [1 3 x]])
     (trueo [:cell [2 2 x]])
     (trueo [:cell [3 1 x]])]))
```