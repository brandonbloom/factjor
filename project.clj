(defproject factjor-cljs "0.2.0"
  :description "A Factor-inspired, concatenative DSL for ClojureScript"
  :url "https://github.com/brandonbloom/factjor"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ^:replace ["-Xmx1g" "-server"]

  :plugins [[lein-cljsbuild "0.3.2"]]

  :dependencies [[org.clojure/clojure "1.5.1"]]

  :source-paths ["src"
                 "clojurescript/src/clj"
                 "clojurescript/src/cljs"]

  :cljsbuild
  {:builds
   [{:id "simple"
     :source-paths ["test"]
     :compiler {:optimizations :simple
                :pretty-print true
                :static-fns true}}
    {:id "advanced"
     :source-paths ["test"]
     :compiler {:optimizations :advanced}}]}

)
