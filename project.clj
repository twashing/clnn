(defproject nn "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [swank-clojure "1.4.2"]
                 [clojure-csv/clojure-csv "2.0.0-alpha1"]]
  :plugins [[lein-swank "1.4.4"]]
  :dev-dependencies [[lein-midje "1.0.10"]
                     [com.stuartsierra/lazytest "1.2.3"]
                     ]
  :repositories {"stuart" "http://stuartsierra.com/maven2"}
)