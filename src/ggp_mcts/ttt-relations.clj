(ns my-sandbox.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))

;; role(a) means that a is a role in the game.
(defn roleo [r]
  (conde
    [(== r :x)]
    [(== r :o)]))

;; index isn't listed as a base relation
;; Maybe it should be part of baseo?
(defn indexo [i]
  (conde
    [(== i 1)]
    [(== i 2)]
    [(== i 3)]))

;; input(r,a) means that a is an action for role r.
(defn inputo [r a]
  (conde
    [(fresh [m n]
       (== a [:mark m n]);;change to [:mark [m n]]??
       (roleo r)
       (indexo m)
       (indexo n))]
    [(== a :noop)
     (roleo r)]))

;; base(p) means that p is a base proposition in the game.
;; base propositions are represented as nested vectors
;; [<NAME> [<ARGS..>]]  
;; maybe use a matche?
;; Maybe this relation isn't necessary given this representation
(defn baseo [p]
  (fresh [m n r]
    (conde
      [(== [:cell [m n :x]] p)
       (indexo m)
       (indexo n)]
      [(== [:cell [m n :o]] p)
       (indexo m)
       (indexo n)]
      [(== [:cell [m n :b]] p)
       (indexo m)
       (indexo n)]
      [(== [:control [r]] p)
       (roleo r)])))

;; init(p) means that the proposition p is true in the initial state.
(defn inito [p]
  (conde
    [(== p [:cell [1 1 :x]])]
    [(== p [:cell [1 2 :x]])]
    [(== p [:cell [1 3 :x]])]
    [(== p [:cell [2 1 :b]])]
    [(== p [:cell [2 2 :b]])]
    [(== p [:cell [2 3 :b]])]
    [(== p [:cell [3 1 :o]])]
    [(== p [:cell [3 2 :b]])]
    [(== p [:cell [3 3 :o]])]
    [(== p [:control [:o]])]))

;; true(p) means that the proposition p is true in the current state.
;; initial true relation is the init relation
;; This should be an input to the system to express state I think
(defn trueo [p]
  (inito p))

;; does(r,a) means that player r performs action a in the current state.
;; This should be an input to the system to express a move is made I think
(defn doeso [r,a]
  (conde
    [(== r :x)
     (== a [:mark 1 1])]
    [(== r :o)
     (== a :noop)]))

;; next(p) means that the proposition p is true in the next state.
(defn nexto [p]
  (conde
    [(fresh [m n r]
       (== p [:cell [m n r]])
       (doeso r [:mark m n])
       (trueo [:cell [m n :b]]))]
    [(fresh [m n w]
       (== p [:cell [m n w]])
       (trueo [:cell [m n w]])
       (!= w :b))]
;;    [(fresh [m n w j k]
;;       (== p [:cell [m n :b]])
;;       (doeso w [:mark j k])
;;       (trueo [:cell [m n :b]])
;;       (!= [j k] [m n]))]
    [(fresh [m n w j k]
       (== p [:cell [m n :b]])
       (doeso w [:mark j k])
       (trueo [:cell [m n :b]])
       (!= m j))]
    [(fresh [m n w j k]
       (== p [:cell [m n :b]])
       (doeso w [:mark j k])
       (trueo [:cell [m n :b]])
       (!= n k))]
    [(== p [:control [:x]])
     (trueo [:control [:o]])]
    [(== p [:control [:o]])
     (trueo [:control [:x]])]))

;; legal(r,a) means it is legal for role r to play action a in the current state.
;; r \memberof {:x :o}
;; a is either [:mark M N] or :noop
(defn legalo [r a]
  (conde 
    [(fresh [x y]
       (== a [:mark x y])
       (trueo [:cell [x y :b]])
       (trueo [:control [r]]))]
    [(== r :x)
     (== a :noop)
     (trueo [:control [:o]])]
    [(== r :o)
     (== a :noop)
     (trueo [:control [:x]])]))

;; Supporting concepts and helpers
(defn rowo [m x]
  (fresh (_);;!! wtf
    (trueo [:cell [m 1 x]])
    (trueo [:cell [m 2 x]])
    (trueo [:cell [m 3 x]])))

(defn columno [n x]
  (fresh (_);;!! wtf
    (trueo [:cell [1 n x]])
    (trueo [:cell [2 n x]])
    (trueo [:cell [3 n x]])))

(defn diagonalo [x]
  (conde
    [(trueo [:cell [1 1 x]])
     (trueo [:cell [2 2 x]])
     (trueo [:cell [3 3 x]])]
    [(trueo [:cell [1 3 x]])
     (trueo [:cell [2 2 x]])
     (trueo [:cell [3 1 x]])]))

(defn lineo [x]
  (fresh [m]
    (!= x :b)
    (conde
      [(rowo m x)]
      [(columno m x)]
      [(diagonalo x)])))

;; goal(r,n) means that player the current state has utility n for player r.
(defn goalo [r n]
  (conde
    [(== r :x)
     (== n 100)
     (lineo :x)
     (nafc lineo :o)]
    [(== r :x)
     (== n 50)
     (nafc lineo :x)
     (nafc lineo :o)]
    [(== r :x)
     (== n 0)
     (nafc lineo :x)
     (lineo :o)]
    [(== r :o)
     (== n 100)
     (nafc lineo :x)
     (lineo :o)]
    [(== r :o)
     (== n 50)
     (nafc lineo :x)
     (nafc lineo :o)]
    [(== r :o)
     (== n 0)
     (lineo :x)
     (nafc lineo :o)]))

;;terminal means that the current state is a terminal state.
(defn openo []
  (fresh [m n]
    (trueo [:cell [m n :b]])))

(defn terminalo []
  (fresh [w]
    (conde
      [(lineo w)]
      [(nafc openo)])));; nafc is experimental!!
