(ns ggp-mcts.ttt-relations-map
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))

;; NOTES
;; Every relation now takes a new arg: env, which is a map
;; that represents the current environment. This allows for doeso
;; and trueo to be rebound at runtime, which is essential for building
;; any search tree.
;; 
;; I transformed ttt_relations.clj to this file with some simple commands:
;; (Ignore "'s in the commends. Those are there for clarity
;; - M-x replace-regex [RET] "(defn [A-Za-z]+ \[" [RET] "\&env " [RET]
;; - Then, for each relation previous defined:
;;   - M-x-% "(<NAME>" [RET] "((get-binding env :<NAME>)" [RET]
;; - Finally, you make a map associating the appropriate keyword with
;;   each relation



;; Returns the relation bound to symbol name in environment env
;; Uses partial application to return a relation that doesn't need
;; an environment argument
(defn get-binding [env name]
  (partial (env (keyword name)) env))

;; role(a) means that a is a role in the game.
(defn roleo [env r]
  (conde
    [(== r :x)]
    [(== r :o)]))

;; index isn't listed as a base relation
;; Maybe it should be part of baseo?
(defn indexo [env i]
  (conde
    [(== i 1)]
    [(== i 2)]
    [(== i 3)]))

;; input(r,a) means that a is an action for role r.
(defn inputo [env r a]
  (conde
    [(fresh [m n]
       (== a [:mark m n]);;change to [:mark [m n]]??
       ((get-binding env :roleo) r)
       ((get-binding env :indexo) m)
       ((get-binding env :indexo) n))]
    [(== a :noop)
     ((get-binding env :roleo) r)]))

;; base(p) means that p is a base proposition in the game.
;; base propositions are represented as nested vectors
;; [<NAME> [<ARGS..>]]  
;; maybe use a matche?
;; Maybe this relation isn't necessary given this representation
(defn baseo [env p]
  (fresh [m n r]
    (conde
      [(== [:cell [m n :x]] p)
       ((get-binding env :indexo) m)
       ((get-binding env :indexo) n)]
      [(== [:cell [m n :o]] p)
       ((get-binding env :indexo) m)
       ((get-binding env :indexo) n)]
      [(== [:cell [m n :b]] p)
       ((get-binding env :indexo) m)
       ((get-binding env :indexo) n)]
      [(== [:control [r]] p)
       ((get-binding env :roleo) r)])))

;; init(p) means that the proposition p is true in the initial state.
(defn inito [env p]
  (conde
    [(== p [:cell [1 1 :b]])]
    [(== p [:cell [1 2 :b]])]
    [(== p [:cell [1 3 :b]])]
    [(== p [:cell [2 1 :b]])]
    [(== p [:cell [2 2 :b]])]
    [(== p [:cell [2 3 :b]])]
    [(== p [:cell [3 1 :b]])]
    [(== p [:cell [3 2 :b]])]
    [(== p [:cell [3 3 :b]])]
    [(== p [:control [:x]])]))

;; true(p) means that the proposition p is true in the current state.
;; initial true relation is the init relation
;; This should be an input to the system to express state I think
(defn trueo [env p]
  ((get-binding env :inito) p))

;; does(r,a) means that player r performs action a in the current state.
;; This should be an input to the system to express a move is made I think
(defn doeso [env r a]
  (conde
    [(== r :x)
     (== a [:mark 1 1])]
    [(== r :o)
     (== a :noop)]))

;; next(p) means that the proposition p is true in the next state.
(defn nexto [env p]
  (conde
    [(fresh [m n r]
       (== p [:cell [m n r]])
       ((get-binding env :doeso) r [:mark m n])
       ((get-binding env :trueo) [:cell [m n :b]]))]
    [(fresh [m n w]
       (== p [:cell [m n w]])
       ((get-binding env :trueo) [:cell [m n w]])
       (!= w :b))]
    [(fresh [m n w j k]
       (== p [:cell [m n :b]])
       ((get-binding env :doeso) w [:mark j k])
       ((get-binding env :trueo) [:cell [m n :b]])
       (!= m j))]
    [(fresh [m n w j k]
       (== p [:cell [m n :b]])
       ((get-binding env :doeso) w [:mark j k])
       ((get-binding env :trueo) [:cell [m n :b]])
       (!= n k))]
    [(== p [:control [:x]])
     ((get-binding env :trueo) [:control [:o]])]
    [(== p [:control [:o]])
     ((get-binding env :trueo) [:control [:x]])]))

;; legal(r,a) means it is legal for role r to play action a in the current state.
;; r \memberof {:x :o}
;; a is either [:mark M N] or :noop
(defn legalo [env r a]
  (conde 
    [(fresh [x y]
       (== a [:mark x y])
       ((get-binding env :trueo) [:cell [x y :b]])
       ((get-binding env :trueo) [:control [r]]))]
    [(== r :x)
     (== a :noop)
     ((get-binding env :trueo) [:control [:o]])]
    [(== r :o)
     (== a :noop)
     ((get-binding env :trueo) [:control [:x]])]))

;; Supporting concepts and helpers
(defn rowo [env m x]
  (fresh (_);;!! wtf
    ((get-binding env :trueo) [:cell [m 1 x]])
    ((get-binding env :trueo) [:cell [m 2 x]])
    ((get-binding env :trueo) [:cell [m 3 x]])))

(defn columno [env n x]
  (fresh (_);;!! wtf
    ((get-binding env :trueo) [:cell [1 n x]])
    ((get-binding env :trueo) [:cell [2 n x]])
    ((get-binding env :trueo) [:cell [3 n x]])))

(defn diagonalo [env x]
  (conde
    [((get-binding env :trueo) [:cell [1 1 x]])
     ((get-binding env :trueo) [:cell [2 2 x]])
     ((get-binding env :trueo) [:cell [3 3 x]])]
    [((get-binding env :trueo) [:cell [1 3 x]])
     ((get-binding env :trueo) [:cell [2 2 x]])
     ((get-binding env :trueo) [:cell [3 1 x]])]))

(defn lineo [env x]
  (fresh [m]
    (!= x :b)
    (conde
      [((get-binding env :rowo) m x)]
      [((get-binding env :columno) m x)]
      [((get-binding env :diagonalo) x)])))

;; goal(r,n) means that player the current state has utility n for player r.
(defn goalo [env r n]
  (conde
    [(== r :x)
     (== n 100)
     ((get-binding env :lineo) :x)
     (nafc (get-binding env :lineo) :o)]
    [(== r :x)
     (== n 50)
     (nafc (get-binding env :lineo) :x)
     (nafc (get-binding env :lineo) :o)]
    [(== r :x)
     (== n 0)
     (nafc (get-binding env :lineo) :x)
     ((get-binding env :lineo) :o)]
    [(== r :o)
     (== n 100)
     (nafc (get-binding env :lineo) :x)
     ((get-binding env :lineo) :o)]
    [(== r :o)
     (== n 50)
     (nafc (get-binding env :lineo) :x)
     (nafc (get-binding env :lineo) :o)]
    [(== r :o)
     (== n 0)
     ((get-binding env :lineo) :x)
     (nafc (get-binding env :lineo) :o)]))

;;terminal means that the current state is a terminal state.
(defn openo [env]
  (fresh [m n]
    ((get-binding env :trueo) [:cell [m n :b]])))

(defn terminalo [env]
  (fresh [w]
    (conde
      [((get-binding env :lineo) w)]
      [(nafc openo)])));; nafc is experimental!!

;; environment built from the above relations
(def ttt-env
  {:roleo roleo
   :indexo indexo
   :inputo inputo
   :baseo baseo
   :inito inito
   :trueo trueo
   :doeso doeso
   :nexto nexto
   :legalo legalo
   :rowo rowo
   :columno columno
   :diagonalo diagonalo
   :lineo lineo
   :goalo goalo
   :openo openo
   :terminalo terminalo})

;; Another doeso where :x marks (2,2) as opposed to (1,1)
(defn alt-doeso [env r a]
  (conde
    [(== r :x)
     (== a [:mark 2 2])]
    [(== r :o)
     (== a :noop)]))

;; And the whole reason I did this:
;;
;; ggp-mcts.core=> (set (run* [q] ((get-binding ttt-env :nexto) q)))
;; #{[:cell [3 3 :b]] [:cell [2 2 :b]] [:cell [3 2 :b]] [:cell [2 1 :b]]
;; [:cell [3 1 :b]] [:control [:o]] [:cell [1 1 :x]] [:cell [1 3 :b]] 
;; [:cell [2 3 :b]] [:cell [1 2 :b]]}
;;
;; ggp-mcts.core=> (set (run* [q] ((get-binding (assoc ttt-env :doeso alt-doeso) 
;;                                              :nexto) q)))
;; #{[:cell [3 3 :b]] [:cell [1 1 :b]] [:cell [3 2 :b]] [:cell [2 1 :b]] 
;; [:cell [3 1 :b]] [:control [:o]] [:cell [2 2 :x]] [:cell [1 3 :b]] 
;; [:cell [2 3 :b]] [:cell [1 2 :b]]}
