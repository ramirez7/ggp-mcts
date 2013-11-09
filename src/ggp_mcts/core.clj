(ns ggp-mcts.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))

;;; This file will house the top-level functions needed for the user to
;;; create a Monte-Carlo Search Tree function for their game as described in
;;; one of three ways:
;;; - Game Description Language
;;; - core.logic relations
;;; - Clojure functions

;;; Environment methods
;;; An environment is a map of the following form:
;;; {:relations {KEY-RELATION PAIRS}, :functions {KEY-FUNCTION PAIRS}}

(defn get-binding [env path]
  (partial (get-in env path) env))

(defn get-relation [env name]
  (get-binding env [:relations name]))

(defn get-function [env name]
  (get-binding env [:functions name]))

;;; ---------------------------------------------------------------------------

;;; Relation->Function transformers
;;; TODO: COMMENT ALL THIS MORE AND EXPLAIN THOROUGHLY
;;; moves is a map with keys as player names, and values of their moves
;;; Right now, it generates code. If I change how moves are represented
;;; it wouldn't be necessary. The does relation woudl have to take an arg
;;; that is a vector of both r and a. Then it'd work.
(defn update-does [env moves]
  (let [next-does (list 'fn ['env 'r 'a] 
                        (concat '(conde) ;;DIRTY. backtick lib would help
                                (map (fn [[k v]]
                                       [(list '== 'r k)
                                        (list '== 'a v)])
                                     moves)))]
    (assoc-in env [:relations :does] (eval next-does))))

(defn create-true [state]
  (fn [env p]
    (membero p state)))

(defn update-true
  "Returns a new environment identical to env except that the
  :does and :true relations are updated as a result of the provided
  moves

  Maybe it should be renamed since it updates :true AND :does"
  [env moves]
  (let [next-state (distinct 
                    (run* [q] 
                      ((get-relation (update-does env moves) :next) q)))]
    (assoc-in env [:relations :true] (create-true next-state))))

;;; minikanren is great for permutations etc
(defn comboo [pls mls c]
  (conde
    [(emptyo pls)
     (emptyo mls)
     (== c '())]
    [(fresh [pa pd ma md choice res]
       (conso pa pd pls)
       (conso ma md mls)
       (membero choice ma)
       (conso [pa choice] res c)
       (comboo pd md res))]))

(defn alist->map
  "Converts association list als to a clojure map.
  Association lists are lists of two-element vectors.

  ([k1 v1] [k2 v2] ..) -> {k1 v1, k2 v2, ..}"
  [als]
  (reduce (fn [m [k v]]
            (assoc m k v)) {} als))

(defn gen-children
  "Returns a list of all possible next-states, as
  represented by environments. This means that :does and :true
  are updated in the outputed environment."
  [env]
  (let [players (run* [q] ((get-relation env :role) q))
        gen-legal (fn [p] 
                    (run* [q]
                      ((get-relation env :legal) p q)))
        legal-moves (zipmap players (map gen-legal players))
        legal-combos (run* [q]
                       (comboo (keys legal-moves) (vals legal-moves) q))
        all-combos (map alist->map legal-combos)]
    (map (partial update-true env) all-combos)))

;;NOTES on gen-children
;;players is a list of all players
;;gen-legal is a function that generates all legal moves for player p
;;legal-moves is a list of maps {PLAYER MOVE}
;;legal-combos is all combinations of legal moves as an assq
;;all-combos is that as maps (COMBINE WITH ABOVE?)
;;Then it's as simple as updating the true in the env for each possible
;; move combination and BOOM all children

(defn terminal?
  "Returns true if the game is over and false if not
  (as determined by the :terminal relation)"
  [env]
  (not (empty? (run* [q] ((get-relation env :terminal))))))


(defn get-scores
  "Returns the score of each player in the env
  as a map {PLAYER, SCORE AS RETURNED BY :goal RELATION}"
  [env]
  (let [players (run* [q] ((get-relation env :role) q))
        score (fn [p]
                (first 
                 (run 1 [q]
                      ((get-relation env :goal) p q))))]
    (zipmap players (map score players))))

;;; Early cut-off helpers
(defn mean
  "Calculates the mean of all members of a collection."
  [coll]
  (/ (reduce + coll) (count coll)))

(defn variance
  "Calculates the variance of all members of a collection."
  [coll]
  (let [u (mean coll)]
    (/ (reduce + 
            (map 
             (fn [n]
               (* (- n u) (- n u)))
             coll))
       (count coll))))

(defn playout
  "playout randomly plays a game from the state in env until
  it terminates and returns the results"
  [env]
  (if (terminal? env)
    (get-scores env)
    (recur (rand-nth (gen-children env)))))

;;; ---------------------------------------------------------------------------

;;; Monte-Carlo Tree Search
;;; NOTES:
;;; I'm using randomcomputation's Clojure MCTS as reference:
;;; http://randomcomputation.blogspot.com/2013/01/
;;; monte-carlo-tree-search-in-clojure.html
;;;
;;; However, because I'm aiming to make it more general, there
;;; are definitely some other considerations and changes to be made.


(def example-statistics
  {#{:whatever} {:scores {:x 200, :o 0 :f 20}
                 :visits 2
                 :children '(CHILDREN)}})

;;; For now, this is just an average.
;;; It's returned in the same format as playout and get-scores
;;; TODO: Implement UCT
(defn mcts-value [stats]
  (zipmap (keys (stats :scores)) 
          (map (fn [n] 
                 (/ n (stats :visits)))
               (vals (stats :scores)))))


;;; ARGS:
;;; RET: map of results
(defn mcts-sample [ARGS]
  'TODO)

;;; ARGS:
;;; RET: move OR state (not sure yet)
;;;      (Will need to keep track of moves somehow)
;;;      (state is likely the best choice here. Leaves moves to top level)
;;;      (state will probably need to be put into env with update-true)
(defn mcts-select [ARGS]
  'TODO)

;;; ARGS:
;;; RET: list of unexplored states (probably as sets/lists of props)
;;;      (state will probably need to be put into env with update-true)
(defn mcts-unexplored [ARGS]
  'TODO)

;;; ARGS:
;;; RET:
(defn mcts-add-child [ARGS]
  'TODO)

;;; ARGS: 
;;; RET: 
(defn mcts-grow [ARGS]
  'TODO)

;;; ARGS:
;;; RET: statistics (I think)
(defn mcts-iteration [ARGS]
  'TODO)

;; MORE
