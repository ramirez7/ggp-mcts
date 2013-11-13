(defproject ggp-mcts "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.8.4"]
                 [org.clojure/core.match "0.2.0"]]
  :jvm-opts ["-XX:MaxPermSize=512M"]
  ;;uncomment these to trace Class (Un)Loading
;             "-XX:+TraceClassLoading" 
;             "-XX:+TraceClassUnloading"]
  :main ggp-mcts.core)
