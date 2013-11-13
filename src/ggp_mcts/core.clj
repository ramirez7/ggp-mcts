(ns ggp-mcts.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use [clojure.core.match :only (match)])
  (:require [clojure.core.logic.fd :as fd])
  (:require clojure.pprint))

;; Memory leak?
;; http://eigenjoy.com/2011/03/02/
;; clojures-keyword-can-fill-up-your-permgen-space/
;; Out of permgen space...
;; maybe it has to do with me calling eval/redefining lambdas?

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

;;; Example GDL relation
;;; Legal from Tic-Tac-Toe
(def ttt-gdl-legal
  '((<= (legal ?w (mark ?x ?y))
        (true (cell ?x ?y b))
        (true (control ?w)))
    (<= (legal white noop)
        (true (control black)))
    (<= (legal black noop)
        (true (control white)))))


;;; Some ways to write clojure code
(def test-fn-list
  (list 'fn ['a 'b] 
        (list 'conde 
              [(list '== 'a 1)] 
              [(list '== 'b 2)])))

(def test-fn-quasi
  (let [fn-sym 'fn
        args ['a 'b]
        conde-sym 'conde
        conde-clauses (list [`(~'== ~(args 0) 1)]
                            [`(~'== ~(args 1) 2)])]
    `(~fn-sym ~args (~conde-sym ~@conde-clauses))))

(defn test-match [e title]
  (match [(vec e)]
    [[title & args]] args
    [[rator & args]] (list args rator)))

;;This also works and has less in let:
;;  `(~'fn ~args (~'conde ~@conde-clauses))

;;; GDL->Relation transformers
(defn fresh-var? [e]
  (and (symbol? e)
       (= \? (first (name e)))))

(defn find-fresh-vars [clause]
  (vec (distinct (filter fresh-var? (flatten clause)))))

(defn symbol->keyword [s]
  (keyword (str s)))

(defn keyword->symbol [k]
  (symbol (apply str (rest (str k)))))
;;; Assumes arg is either symbol or list of symbols
;;; Transforms lists into vecs so core.logic can do cool stuff with them
;;; Leaves fresh-variables as symbols
;;; If it isn't a fresh variable, 
(defn transform-arg [arg]
  (if (list? arg)
    (vec
     (map (fn [a]
            (if (or (fresh-var? a) (number? a))
              a
              (keyword (str a)))) arg))
    (if (or (fresh-var? arg) (number? arg))
      arg
      (keyword (str arg)))))

;;; Takes a GDL relation and returns a *list* of core.logic
;;; relations that correspond to it. "Head" relations correspond
;;; to lists of (== a b) unifications. Other relations refer to
;;; calling external relations, which requires an environment.

(defn transform-call [caller-name [env & caller-args] call]
  (let [clean-call (if (list? call) (vec call) [call])
        clean-name (keyword->symbol caller-name)]
    (match [clean-call]
      [[(r :guard symbol?)]] '()
      [['not (r :guard symbol?)]]
      (list `(~'nafc (~'get-relation ~env ~(symbol->keyword r))))
      [['not (r :guard list?)]] 
      (list `(~'nafc (~'get-relation ~env ~(symbol->keyword (first r))) 
                     ~@(map transform-arg (rest r))))
      [[clean-name & args]]
      (map (fn [p v] `(~'== ~p ~v))
           caller-args
           (map transform-arg args))
      [[callee-name & args]]
      (list `((~'get-relation ~env ~(symbol->keyword callee-name))
              ~@(map transform-arg args))))))


(defn transform-clause [caller-name caller-args clause]
  (match [(vec clause)]
    [['<= & clauses]]
    [`(~'fresh ~(find-fresh-vars clause)
        ~@(mapcat (partial transform-call caller-name caller-args) clauses))]
    :else
    [`(~'fresh ~(find-fresh-vars clause)
        ~@(transform-call caller-name caller-args clause))]))
  
(defn transform-relation [relation-name relation-args relation]
  (let [clauses (map (partial transform-clause
                              relation-name
                              relation-args) relation)]
    `(~'fn ~relation-args (~'conde ~@clauses))))

;;; This is verbatim what
;;; (transform-relation :legal ['env 'r 'm] ttt-gdl-legal) created.
;;;
;;; And it works! (I had to make a special environment in which
;;; propositions were structured as below though)
(def ttt-gdl-legal-transformed
  (fn [env r m]
    (conde 
      [(fresh [?w ?x ?y] 
         (== r ?w) 
         (== m [:mark ?x ?y]) 
         ((get-relation env :true) [:cell ?x ?y :b]) 
         ((get-relation env :true) [:control ?w]))]
      [(fresh [] 
         (== r :white) 
         (== m :noop) 
         ((get-relation env :true) [:control :black]))] 
      [(fresh [] 
         (== r :black) 
         (== m :noop) 
         ((get-relation env :true) [:control :white]))])))

(def initial-env
  {:relations
   {:distinct (fn [env a b] (distincto [a b]))}})

;; TODO: use core.match instead of these nested ifs

(defn find-relation-name [relation]
  (if (= (first relation) '<=)
    (if (symbol? (second relation))
      (keyword (str (second relation)))
      (keyword (str (first (second relation)))))
    (keyword (str (first relation)))))

(defn gen-relation-args [relation]
  (let [relation-args (if (= (first relation) '<=)
                        (if (symbol? (second relation))
                          []
                          (repeatedly (count (rest (second relation))) gensym))
                        (repeatedly (count (rest relation)) gensym))]
    (vec (cons 'env relation-args))))

      
;;; description is a list of GDL relations in prefix notation
(defn collect-relations [description]
  (let [collector (fn [m r]
                    (-> m
                        (update-in
                         [(find-relation-name r) :body]
                         (fn [old new]
                           (if old (cons new old) (cons new '())))
                         r)
                        (update-in
                         [(find-relation-name r) :args]
                          (fn [old new]
                            (if old old (gen-relation-args new)))
                          r)))]
    (reduce collector {} description)))

(defn map-map [f m]
  (reduce (fn [m [k v]]
            (assoc m k (f v))) {} m))

(defn create-code [description]
  (reduce
   (fn [m [relation-name {args :args body :body}]]
     (assoc-in m [relation-name] (transform-relation relation-name args body)))
   {}
   (collect-relations description)))

(defn create-environment [description]
  (let [code (create-code description)
        env (update-in
             initial-env [:relations] conj
             (map-map (fn [e] (eval e)) code))]
    (assoc-in env [:relations :true] (get-in env [:relations :init]))))

;;; Notes:
;;; distinct is in GDL. It maps directly to distincto
;;; It'd probably be easiest to just map a key :distinct to distincto in the env
;;;
;;; (not (rator args)) -~-> (nafc (get-relation env rator) & args)



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


(defn initial-stats [env]
  (let [players (run* [q] ((get-relation env :role) q))
        scores (zipmap players (repeat (count players) 0))]
    {:scores scores
     :visits 0
     :children (gen-children env)}))

;;; Returns the "state" (as a set, which can be used in the 
;;; statistics map as a key)
(defn get-state [env]
  (set (run* [q] ((get-relation env :true) q))))

;;; Gets the statistics map of the current environment
(defn get-stats [env stats]
  (get stats (get-state env) (initial-stats env)))

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
(defn mcts-sample [env n]
  (loop [result ((initial-stats env) :scores) n n]
    (if (< n 1) result
        (recur (zipmap
                (keys result) 
                (map (fn [k]
                       (+ ((playout env) k) (result k)))
                     (keys result)))
                                        ;                (map + (vals result) (vals (playout env))))
               (dec n)))))

;;; path is a list of environments
(defn mcts-backprop [path result stats]
  (if (empty? path)
    stats
    (let [factor (if (terminal? (first path)) 1 1) ;;DIRTY
          state (get-state (first path))
          prev-scores ((get-stats (first path) stats) :scores)
          next-scores (zipmap 
                       (keys prev-scores) 
                       (map (fn [k]
                              (let [s (if (= (result k) (apply max (vals result)))
                                        ;; makes terminal winner "stick out"
                                        ;; IS THIS ACTUALLY OKAY THOUGH?
                                        ;; COULD THERE BE A DOWNSIDE/BAD BIAS?
                                        (* factor (result k))
                                        (result k))]
                                (+ (prev-scores k)
                                   s)))
                            (keys prev-scores)))]
      (recur (rest path) result 
             (-> stats
                 (assoc-in [state :scores] next-scores)
                 (update-in [state :visits] inc))))))

;;; ARGS:
;;; RET: move OR state (not sure yet)
;;;      (Will need to keep track of moves somehow)
;;;      (state is likely the best choice here. Leaves moves to top level)
;;;      (state will probably need to be put into env with update-true)
;;; 
;;;  This is the only function that cares about whose turn it is.
(defn mcts-select [env stats player]
  (let [state (get-state env)
        children (get-in stats [state :children])
        explored (remove
                  #(zero? ((get-stats % stats) :visits))
                  children)]
    (if (empty? explored)
      (rand-nth children)
      (apply max-key #((mcts-value (get-stats % stats)) player) explored))))

;;; ARGS:
;;; RET: list of unexplored states (probably as sets/lists of props)
;;;      (state will probably need to be put into env with update-true)
(defn mcts-unexplored [env stats]
  (let [state (get-state env)
        children (get-in stats [state :children] (gen-children env))
        unexplored? (fn [cenv]
                      (= 0 (get-in stats [(get-state cenv) :visits] 0)))]
    (filter unexplored? children)))


;;; Adds state in env to stats
;;; Initializes child notes for the stat
;;; If the state is already in stats, it remains untouched
;;; (it won't reset to initial)
;;; ARGS:
;;; RET:
(defn mcts-add-child [env stats]
  (let [state (get-state env)
        child-stats (get stats state (initial-stats env))]
    (assoc stats state child-stats)))

;;; ARGS: 
;;; RET: 
(defn mcts-grow [stats path]
  (let [leaf (first path)
        child (rand-nth (mcts-unexplored leaf stats))
        result (mcts-sample child 1)]
    (->> stats
         (mcts-add-child child)
         (mcts-backprop (cons child path) result))))

;;; ARGS:
;;; RET: statistics (I think)
(defn mcts-iteration [env stats player]
  (loop [env env, stats stats, path (list env)]
    (if (terminal? env)
      (mcts-backprop path (get-scores env) stats)
      (if (not-empty (mcts-unexplored env stats))
        (mcts-grow stats path)
        (let [ch (mcts-select env stats player)]
          (recur ch stats (cons ch path)))))))

(defn mcts-algorithm [env player budget]
  (loop [env env
         stats {(get-state env) (initial-stats env)}
         budget budget]
    (if (< budget 1) stats
        (recur env (mcts-iteration env stats player) (dec budget)))))

(defn find-move [env next-env player]
  (let [players (run* [q] ((get-relation env :role) q))
        gen-legal (fn [p]
                    (run* [q]
                          ((get-relation env :legal) p q)))
        legal-moves (zipmap players (map gen-legal players))
        legal-combos (run* [q]
                           (comboo (keys legal-moves) (vals legal-moves) q))
        all-combos (map alist->map legal-combos)
        found-combo (take 1 (filter (fn [c]
                                      (= (get-state (update-true env c))
                                         (get-state next-env)))
                                    all-combos))]
    ((first found-combo) player)))

(defn child-scores [env budget player]
  (let [stats (mcts-algorithm env player budget)]
    (map (fn [e]
           [(terminal? e) (get-scores e)
            (find-move env e player)
            (get-in stats [(get-state e) :visits])
            (get-in stats [(get-state e) :scores])])
         (gen-children env))))


(defn best-move [env player budget]
  (find-move
   env (mcts-select
        env (mcts-algorithm env player budget) player) player))


#_(defn learn-iteration [mem state]
    "The core algorithm; a single analysis of a state. Searches the tree
for an unexplored child, estimates the child's value, adds
it to the tree, and backpropagates the value up the path."
    (loop [mem mem, state state, path (list state)]
      (if-let [result (ttt-terminal? state)]
        (uct-backprop mem path result)
        (if (not-empty (uct-unexplored mem state))
          (uct-grow mem path)
          (let [ch (uct-select mem state)]
            (recur mem ch (cons ch path)))))))
;; MORE

;; I suspect there's a space leak somewhere.
;; REPL randomly runs out of memory? hm

;;; play-game 
;;; players must be ternary functions with args [env player budget]
;;; and return a move
;;; the play-game function then update the state according to moves
;;;
;;; Right now, it's just 2-players to test TicTacToe.
;;; TODO: Make it with n-player games
;;;
;;; It also prints
(defn play-game [env player1 player2 budget]
  (let [x-player (fn [env] (player1 env :x budget))
        o-player (fn [env] (player2 env :o budget))]
    (loop [env env turn 0]
      (do
        (printf "Turn %s\n" turn)
        (pprint (run* [q] ((get-relation env :true) q)))
        (if (terminal? env)
          (get-scores env)
          (recur (update-true env
                              {:x (x-player env)
                               :o (o-player env)})
                 (inc turn)))))))

;;; Random player
(defn random-move [env player _]
  (rand-nth (run* [q] ((get-relation env :legal) player q))))

;; -------------------------------------------------------------------------
;; From https://gist.github.com/jsmorph/7042092
;; Ugly mitigation of http://dev.clojure.org/jira/browse/CLJ-1152 in
;; the protocols case. Motivation: Need to eval lots domain-specific
;; language code rendered in core.logic. Approach: Start a thread to
;; purge protocol caches every second. You should probable write a
;; fancier version.

(defn protocol? [x]
  (and (instance? clojure.lang.PersistentArrayMap x)
       (boolean (:on-interface x))))

(defn protocols
  "Get a seq of protocols in the given namespace."
  ([namespace]
     (binding [*ns* (if (symbol? namespace)
                      (find-ns namespace)
                      namespace)]
       (doall (filter protocol?
                      (map var-get
                           (filter var?
                                   (map second
                                        (ns-map *ns*))))))))
  ([]
     (protocols *ns*)))

(defonce simple-protocol-cache-cleaner
  ;; Starts a simple daemon thread that purges the caches for
  ;; all protocols in the current namespace.
  (let [interval 1000 ;; ms
        ps (protocols *ns*)]
    (doto (new Thread (fn []
                        (loop [i 0]
                          (doseq [p ps] (-reset-methods p))
                          (Thread/sleep interval)
                          (recur (inc i)))))
      (.setDaemon true)
      (.start))))
