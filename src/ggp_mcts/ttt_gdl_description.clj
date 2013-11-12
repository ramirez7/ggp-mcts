(ns ggp-mcts.ttt-gdl-description)

(def gdl
  '(

    (role white)
    (role black)

    (<= (base (cell ?m ?n x)) (index ?m) (index ?n))
    (<= (base (cell ?m ?n o)) (index ?m) (index ?n))
    (<= (base (cell ?m ?n b)) (index ?m) (index ?n))
    (base (control white))
    (base (control black))

    (<= (input ?r (mark ?m ?n)) (role ?r) (index ?m) (index ?n))
    (<= (input ?r noop) (role ?r))

    (index 1)
    (index 2)
    (index 3)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init (cell 1 1 b))
    (init (cell 1 2 b))
    (init (cell 1 3 b))
    (init (cell 2 1 b))
    (init (cell 2 2 b))
    (init (cell 2 3 b))
    (init (cell 3 1 b))
    (init (cell 3 2 b))
    (init (cell 3 3 b))
    (init (control white))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; legal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (<= (legal ?w (mark ?x ?y))
        (true (cell ?x ?y b))
        (true (control ?w)))
    
    (<= (legal white noop)
        (true (control black)))
    
    (<= (legal black noop)
        (true (control white)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; next
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (<= (next (cell ?m ?n x))
        (does white (mark ?m ?n))
        (true (cell ?m ?n b)))
    
    (<= (next (cell ?m ?n o))
        (does black (mark ?m ?n))
        (true (cell ?m ?n b)))
    
    (<= (next (cell ?m ?n ?w))
        (true (cell ?m ?n ?w))
        (distinct ?w b))
    
    (<= (next (cell ?m ?n b))
        (does ?w (mark ?j ?k))
        (true (cell ?m ?n b))
        (distinct ?m ?j))
    
    (<= (next (cell ?m ?n b))
        (does ?w (mark ?j ?k))
        (true (cell ?m ?n b))
        (distinct ?n ?k))
    
    (<= (next (control white))
        (true (control black)))
    
    (<= (next (control black))
        (true (control white)))
    
    
    (<= (row ?m ?x)
        (true (cell ?m 1 ?x))
        (true (cell ?m 2 ?x))
        (true (cell ?m 3 ?x)))
    
    (<= (column ?n ?x)
        (true (cell 1 ?n ?x))
        (true (cell 2 ?n ?x))
        (true (cell 3 ?n ?x)))
    
    (<= (diagonal ?x)
        (true (cell 1 1 ?x))
        (true (cell 2 2 ?x))
        (true (cell 3 3 ?x)))
    
    (<= (diagonal ?x)
        (true (cell 1 3 ?x))
        (true (cell 2 2 ?x))
        (true (cell 3 1 ?x)))
    
    
    (<= (line ?x) (row ?m ?x))
    (<= (line ?x) (column ?m ?x))
    (<= (line ?x) (diagonal ?x))
    
    
    (<= open (true (cell ?m ?n b)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; goal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (<= (goal white 100)
        (line x)
        (not (line o)))
    
    (<= (goal white 50)
        (not (line x))
        (not (line o)))
    
    (<= (goal white 0)
        (not (line x))
        (line o))

    (<= (goal black 100)
        (not (line x))
        (line o))
    
    (<= (goal black 50)
        (not (line x))
        (not (line o)))
    
    (<= (goal black 0)
        (line x)
        (not (line o)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (<= terminal
        (line x))
    
    (<= terminal
        (line o))
    
    (<= terminal
        (not open))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
