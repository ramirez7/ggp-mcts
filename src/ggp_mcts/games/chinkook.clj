(ns ggp-mcts.games.chinkook)

(def gdl
  '(

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chinook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (role white)
    (role black)


    (<= (base (oddcell ?f ?r ?player))
        (isaodd ?f ?r)
        (role ?player))

    (<= (base (oddcell ?f ?r b))
        (isaodd ?f ?r))

    (<= (base (oddscore ?player 0))
        (role ?player))

    (<= (base (oddscore ?player ?n))
        (role ?player)
        (scoreplus ?m ?n))

    (<= (base (oddcontrol ?player))
        (role ?player))

    (<= (base (oddstep ?m))
        (succ ?m ?n))

    (base (oddstep 41))

    (<= (base (evencell ?f ?r ?player))
        (isaeven ?f ?r)
        (role ?player))

    (<= (base (evencell ?f ?r b))
        (isaeven ?f ?r))

    (<= (base (evenscore ?player 0))
        (role ?player))

    (<= (base (evenscore ?player ?n))
        (role ?player)
        (scoreplus ?m ?n))

    (<= (base (evencontrol ?player))
        (role ?player))

    (<= (base (evenstep ?m))
        (succ ?m ?n))

    (base (evenstep 41))


    (<= (input white (oddmove ?u ?v ?x ?y))
        (isaodd ?u ?v)
        (whitemove ?u ?v ?x ?y))

    (<= (input white (oddjump ?u ?v ?c ?d ?x ?y))
        (isaodd ?u ?v)
        (whitejump ?u ?v ?c ?d ?x ?y))

    (<= (input black (oddmove ?u ?v ?x ?y))
        (isaodd ?u ?v)
        (blackmove ?u ?v ?x ?y))

    (<= (input black (oddjump ?u ?v ?c ?d ?x ?y))
        (isaodd ?u ?v)
        (blackjump ?u ?v ?c ?d ?x ?y))

    (<= (input ?player oddnoop)
        (role ?player))

    (<= (input white (evenmove ?u ?v ?x ?y))
        (isaeven ?u ?v)
        (whitemove ?u ?v ?x ?y))

    (<= (input white (evenjump ?u ?v ?c ?d ?x ?y))
        (isaeven ?u ?v)
        (whitejump ?u ?v ?c ?d ?x ?y))

    (<= (input black (evenmove ?u ?v ?x ?y))
        (isaeven ?u ?v)
        (blackmove ?u ?v ?x ?y))

    (<= (input black (evenjump ?u ?v ?c ?d ?x ?y))
        (isaeven ?u ?v)
        (blackjump ?u ?v ?c ?d ?x ?y))

    (<= (input ?player evennoop)
        (role ?player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (init (oddcell  a 1 white))
    (init (evencell a 2 white))
    (init (oddcell  a 3 b))
    (init (evencell a 4 b))
    (init (oddcell  a 5 b))
    (init (evencell a 6 b))
    (init (oddcell  a 7 black))
    (init (evencell a 8 black))
    (init (evencell b 1 white))
    (init (oddcell  b 2 white))
    (init (evencell b 3 b))
    (init (oddcell  b 4 b))
    (init (evencell b 5 b))
    (init (oddcell  b 6 b))
    (init (evencell b 7 black))
    (init (oddcell  b 8 black))
    (init (oddcell  c 1 white))
    (init (evencell c 2 white))
    (init (oddcell  c 3 b))
    (init (evencell c 4 b))
    (init (oddcell  c 5 b))
    (init (evencell c 6 b))
    (init (oddcell  c 7 black))
    (init (evencell c 8 black))
    (init (evencell d 1 white))
    (init (oddcell  d 2 white))
    (init (evencell d 3 b))
    (init (oddcell  d 4 b))
    (init (evencell d 5 b))
    (init (oddcell  d 6 b))
    (init (evencell d 7 black))
    (init (oddcell  d 8 black))
    (init (oddcell  e 1 white))
    (init (evencell e 2 white))
    (init (oddcell  e 3 b))
    (init (evencell e 4 b))
    (init (oddcell  e 5 b))
    (init (evencell e 6 b))
    (init (oddcell  e 7 black))
    (init (evencell e 8 black))
    (init (evencell f 1 white))
    (init (oddcell  f 2 white))
    (init (evencell f 3 b))
    (init (oddcell  f 4 b))
    (init (evencell f 5 b))
    (init (oddcell  f 6 b))
    (init (evencell f 7 black))
    (init (oddcell  f 8 black))
    (init (oddcell  g 1 white))
    (init (evencell g 2 white))
    (init (oddcell  g 3 b))
    (init (evencell g 4 b))
    (init (oddcell  g 5 b))
    (init (evencell g 6 b))
    (init (oddcell  g 7 black))
    (init (evencell g 8 black))
    (init (evencell h 1 white))
    (init (oddcell  h 2 white))
    (init (evencell h 3 b))
    (init (oddcell  h 4 b))
    (init (evencell h 5 b))
    (init (oddcell  h 6 b))
    (init (evencell h 7 black))
    (init (oddcell  h 8 black))

    (init (oddscore white 0))
    (init (oddscore black 0))	
    (init (evenscore white 0))
    (init (evenscore black 0))	

    (init (oddcontrol white))
    (init (evencontrol black))

    (init (oddstep 1))
    (init (evenstep 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; legal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (<= (legal white (oddjump ?u ?v ?c ?d ?x ?y))
        (true (oddcontrol white))
        (true (oddcell ?u ?v white))
        (whitejump ?u ?v ?c ?d ?x ?y)
        (true (oddcell ?c ?d black))
        (true (oddcell ?x ?y b)))

    (<= (legal white (oddmove ?u ?v ?x ?y))
        (true (oddcontrol white))
        (true (oddcell ?u ?v white))
        (whitemove ?u ?v ?x ?y)
        (true (oddcell ?x ?y b)))

    (<= (legal black (oddjump ?u ?v ?c ?d ?x ?y))
        (true (oddcontrol black))
        (true (oddcell ?u ?v black))
        (blackjump ?u ?v ?c ?d ?x ?y)
        (true (oddcell ?c ?d white))
        (true (oddcell ?x ?y b)))

    (<= (legal black (oddmove ?u ?v ?x ?y))
        (true (oddcontrol black))
        (true (oddcell ?u ?v black))
        (blackmove ?u ?v ?x ?y)
        (true (oddcell ?x ?y b)))

    (<= (legal ?player oddnoop)
        (true (oddcontrol ?player)))

    (<= (legal white (evenjump ?u ?v ?c ?d ?x ?y))
        (true (evencontrol white))
        (true (evencell ?u ?v white))
        (whitejump ?u ?v ?c ?d ?x ?y)
        (true (evencell ?c ?d black))
        (true (evencell ?x ?y b)))

    (<= (legal white (evenmove ?u ?v ?x ?y))
        (true (evencontrol white))
        (true (evencell ?u ?v white))
        (whitemove ?u ?v ?x ?y)
        (true (evencell ?x ?y b)))

    (<= (legal black (evenjump ?u ?v ?c ?d ?x ?y))
        (true (evencontrol black))
        (true (evencell ?u ?v black))
        (blackjump ?u ?v ?c ?d ?x ?y)
        (true (evencell ?c ?d white))
        (true (evencell ?x ?y b)))

    (<= (legal black (evenmove ?u ?v ?x ?y))
        (true (evencontrol black))
        (true (evencell ?u ?v black))
        (blackmove ?u ?v ?x ?y)
        (true (evencell ?x ?y b)))

    (<= (legal ?player evennoop)
        (true (evencontrol ?player)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; next
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (<= (next (oddcell ?u ?v b))
        (does ?player (oddmove ?u ?v ?x ?y)))

    (<= (next (oddcell ?x ?y ?player))
        (does ?player (oddmove ?u ?v ?x ?y)))

    (<= (next (oddcell ?m ?n ?p))
        (does ?player (oddmove ?u ?v ?x ?y))
        (true (oddcell ?m ?n ?p))
        (different ?u ?v ?m ?n)
        (different ?x ?y ?m ?n))

    (<= (next (oddcell ?u ?v b))
        (does ?player (oddjump ?u ?v ?c ?d ?x ?y)))

    (<= (next (oddcell ?c ?d b))
        (does ?player (oddjump ?u ?v ?c ?d ?x ?y)))

    (<= (next (oddcell ?x ?y ?player))
        (does ?player (oddjump ?u ?v ?c ?d ?x ?y)))

    (<= (next (oddcell ?m ?n ?p))
        (does ?player (oddjump ?u ?v ?c ?d ?x ?y))
        (true (oddcell ?m ?n ?p))
        (different ?u ?v ?m ?n)
        (different ?c ?d ?m ?n)
        (different ?x ?y ?m ?n))

    (<= (next (oddcell ?m ?n ?p))
        (true (oddcontrol ?player))
        (does ?player oddnoop)
        (true (oddcell ?m ?n ?p)))

    (<= (next (oddscore ?player ?count))
        (true (oddscore ?player ?count))
        (not (true (oddcontrol ?player))))

    (<= (next (oddscore ?player ?count))
        (true (oddscore ?player ?count))
        (does ?player (oddmove ?u ?v ?x ?y)))

    (<= (next (oddscore ?player ?count2))
        (true (oddscore ?player ?count1))
        (does ?player (oddjump ?u ?v ?c ?d ?x ?y))
        (scoreplus ?count1 ?count2))

    (<= (next (oddscore ?player ?count))
        (true (oddscore ?player ?count))
        (does ?player oddnoop))

    (<= (next (oddcontrol white))
        (true (oddcontrol black)))

    (<= (next (oddcontrol black))
        (true (oddcontrol white)))

    (<= (next (oddstep ?n))
        (true (oddstep ?m))
        (succ ?m ?n))


    (<= (next (evencell ?u ?v b))
        (does ?player (evenmove ?u ?v ?x ?y)))

    (<= (next (evencell ?x ?y ?player))
        (does ?player (evenmove ?u ?v ?x ?y)))

    (<= (next (evencell ?m ?n ?p))
        (does ?player (evenmove ?u ?v ?x ?y))
        (true (evencell ?m ?n ?p))
        (different ?u ?v ?m ?n)
        (different ?x ?y ?m ?n))

    (<= (next (evencell ?u ?v b))
        (does ?player (evenjump ?u ?v ?c ?d ?x ?y)))

    (<= (next (evencell ?c ?d b))
        (does ?player (evenjump ?u ?v ?c ?d ?x ?y)))

    (<= (next (evencell ?x ?y ?player))
        (does ?player (evenjump ?u ?v ?c ?d ?x ?y)))

    (<= (next (evencell ?m ?n ?p))
        (does ?player (evenjump ?u ?v ?c ?d ?x ?y))
        (true (evencell ?m ?n ?p))
        (different ?u ?v ?m ?n)
        (different ?c ?d ?m ?n)
        (different ?x ?y ?m ?n))

    (<= (next (evencell ?m ?n ?p))
        (true (evencontrol ?player))
        (does ?player evennoop)
        (true (evencell ?m ?n ?p)))

    (<= (next (evenscore ?player ?count))
        (true (evenscore ?player ?count))
        (not (true (evencontrol ?player))))

    (<= (next (evenscore ?player ?count))
        (true (evenscore ?player ?count))
        (does ?player (evenmove ?u ?v ?x ?y)))

    (<= (next (evenscore ?player ?count2))
        (true (evenscore ?player ?count1))
        (does ?player (evenjump ?u ?v ?c ?d ?x ?y))
        (scoreplus ?count1 ?count2))

    (<= (next (evenscore ?player ?count))
        (true (evenscore ?player ?count))
        (does ?player evennoop))

    (<= (next (evencontrol white))
        (true (evencontrol black)))

    (<= (next (evencontrol black))
        (true (evencontrol white)))

    (<= (next (evenstep ?n))
        (true (evenstep ?m))
        (succ ?m ?n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; goal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (<= (goal white ?score)
        (true (oddscore white ?score)))

    (<= (goal black ?score)
        (true (evenscore black ?score)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (<= terminal
        (true (oddscore ?player 100)))

    (<= terminal
        (true (evenscore ?player 100)))

    (<= terminal
        (true (oddstep 41)))

    (<= terminal
        (true (evenstep 41)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (<= (whitemove ?u ?v ?x ?y)
        (nextrank ?v ?y)
        (nextfile ?u ?x))

    (<= (whitemove ?u ?v ?x ?y)
        (nextrank ?v ?y)
        (nextfile ?x ?u))
    
    (<= (blackmove ?u ?v ?x ?y)
        (nextrank ?y ?v)
        (nextfile ?u ?x))

    (<= (blackmove ?u ?v ?x ?y)
        (nextrank ?y ?v)
        (nextfile ?x ?u))

    (<= (whitejump ?u ?v ?x1 ?y1 ?x ?y)
        (nextrank ?v ?y1)
        (nextrank ?y1 ?y)
        (nextfile ?u ?x1)
        (nextfile ?x1 ?x))

    (<= (whitejump ?u ?v ?x1 ?y1 ?x ?y)
        (nextrank ?v ?y1)
        (nextrank ?y1 ?y)
        (nextfile ?x ?x1)
        (nextfile ?x1 ?u))

    (<= (blackjump ?u ?v ?x1 ?y1 ?x ?y)
        (nextrank ?y ?y1)
        (nextrank ?y1 ?v)
        (nextfile ?u ?x1)
        (nextfile ?x1 ?x))

    (<= (blackjump ?u ?v ?x1 ?y1 ?x ?y)
        (nextrank ?y ?y1)
        (nextrank ?y1 ?v)
        (nextfile ?x ?x1)
        (nextfile ?x1 ?u))

    (<= (different ?x1 ?y1 ?x2 ?y2)
        (isaodd ?x1 ?y1)
        (isaodd ?x2 ?y2)
        (distinct ?x1 ?x2))

    (<= (different ?x1 ?y1 ?x2 ?y2)
        (isaodd ?x1 ?y1)
        (isaodd ?x2 ?y2)
        (distinct ?y1 ?y2))

    (<= (different ?x1 ?y1 ?x2 ?y2)
        (isaeven ?x1 ?y1)
        (isaeven ?x2 ?y2)
        (distinct ?x1 ?x2))

    (<= (different ?x1 ?y1 ?x2 ?y2)
        (isaeven ?x1 ?y1)
        (isaeven ?x2 ?y2)
        (distinct ?y1 ?y2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (nextrank 1 2)
    (nextrank 2 3)
    (nextrank 3 4)
    (nextrank 4 5)
    (nextrank 5 6)
    (nextrank 6 7)
    (nextrank 7 8)
    (nextrank 8 1)

    (nextfile a b)
    (nextfile b c)
    (nextfile c d)
    (nextfile d e)
    (nextfile e f)
    (nextfile f g)
    (nextfile g h)
    (nextfile h a)

    (isaodd  a 1)
    (isaeven a 2)
    (isaodd  a 3)
    (isaeven a 4)
    (isaodd  a 5)
    (isaeven a 6)
    (isaodd  a 7)
    (isaeven a 8)
    (isaeven b 1)
    (isaodd  b 2)
    (isaeven b 3)
    (isaodd  b 4)
    (isaeven b 5)
    (isaodd  b 6)
    (isaeven b 7)
    (isaodd  b 8)
    (isaodd  c 1)
    (isaeven c 2)
    (isaodd  c 3)
    (isaeven c 4)
    (isaodd  c 5)
    (isaeven c 6)
    (isaodd  c 7)
    (isaeven c 8)
    (isaeven d 1)
    (isaodd  d 2)
    (isaeven d 3)
    (isaodd  d 4)
    (isaeven d 5)
    (isaodd  d 6)
    (isaeven d 7)
    (isaodd  d 8)
    (isaodd  e 1)
    (isaeven e 2)
    (isaodd  e 3)
    (isaeven e 4)
    (isaodd  e 5)
    (isaeven e 6)
    (isaodd  e 7)
    (isaeven e 8)
    (isaeven f 1)
    (isaodd  f 2)
    (isaeven f 3)
    (isaodd  f 4)
    (isaeven f 5)
    (isaodd  f 6)
    (isaeven f 7)
    (isaodd  f 8)
    (isaodd  g 1)
    (isaeven g 2)
    (isaodd  g 3)
    (isaeven g 4)
    (isaodd  g 5)
    (isaeven g 6)
    (isaodd  g 7)
    (isaeven g 8)
    (isaeven h 1)
    (isaodd  h 2)
    (isaeven h 3)
    (isaodd  h 4)
    (isaeven h 5)
    (isaodd  h 6)
    (isaeven h 7)
    (isaodd  h 8)

    (scoreplus  0  12)
    (scoreplus 12  25)
    (scoreplus 25  37)
    (scoreplus 37  50)
    (scoreplus 50  62)
    (scoreplus 62  75)
    (scoreplus 75  87)
    (scoreplus 87 100)

    (succ 1 2)
    (succ 2 3)
    (succ 3 4)
    (succ 4 5)
    (succ 5 6)
    (succ 6 7)
    (succ 7 8)
    (succ 8 9)
    (succ 9 10)
    (succ 10 11)
    (succ 11 12)
    (succ 12 13)
    (succ 13 14)
    (succ 14 15)
    (succ 15 16)
    (succ 16 17)
    (succ 17 18)
    (succ 18 19)
    (succ 19 20)
    (succ 20 21)
    (succ 21 22)
    (succ 22 23)
    (succ 23 24)
    (succ 24 25)
    (succ 25 26)
    (succ 26 27)
    (succ 27 28)
    (succ 28 29)
    (succ 29 30)
    (succ 30 31)
    (succ 31 32)
    (succ 32 33)
    (succ 33 34)
    (succ 34 35)
    (succ 35 36)
    (succ 36 37)
    (succ 37 38)
    (succ 38 39)
    (succ 39 40)
    (succ 40 41)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


