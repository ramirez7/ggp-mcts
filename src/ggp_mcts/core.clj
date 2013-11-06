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

;;; Relation->Function transformers

;;; make-move [env move] (key :make-move)
;;;   Updates the environment as the result of the move
;;;   This results in a modified trueo relation
;;;   The doeso relation is first modified to update state with nexto

;;; TODO: COMMENT ALL THIS MORE AND EXPLAIN THOROUGHLY
;;; moves is a map with keys as player names, and values of their moves
;;; Right now, it generates code. If I change how moves are represented
;;; it wouldn't be necessary. The does relation woudl have to take an arg
;;; that is a vector of both r and a. Then it'd work.
(defn update-does [env moves]
  (let [next-does (list 'fn ['env 'r 'a] (concat '(conde) ;;DIRTY
                                                 (map (fn [kv]
                                                        [(list '== 'r (kv 0))
                                                         (list '== 'a (kv 1))])
                                                      moves)))]
    (assoc-in env [:relations :does] (eval next-does))))

(defn create-true [state]
  (fn [env p]
    (membero p state)))

(defn update-true [env moves]
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

(defn assq->map [aq]
  (reduce (fn [m v]
            (assoc m (v 0) (v 1))) {} aq))

(defn gen-children [env]
  ;;players is a list of all players
  ;;gen-legal is a function that generates all legal moves for player p
  ;;legal-moves is a list of maps {PLAYER MOVE}
  ;;legal-combos is all combinations of legal moves as an assq
  ;;all-combos is that as maps (COMBINE WITH ABOVE?)
  ;;Then it's as simple as updating the true in the env for each possible
  ;; move combination and BOOM all children
  (let [players (run* [q] ((get-relation env :role) q))
        gen-legal (fn [p] 
                    (run* [q]
                      ((get-relation env :legal) p q)))
        legal-moves (zipmap players (map gen-legal players))
        legal-combos (run* [q]
                       (comboo (keys legal-moves) (vals legal-moves) q))
        all-combos (map assq->map legal-combos)]
    (map (partial update-true env) all-combos)))



