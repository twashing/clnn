(defproject nn "1.0.0-SNAPSHOT"
  :description "This is me dablling in Neural Networks; Trying to build a predicotr for financial time series-"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [swank-clojure "1.4.2"]
                 [clojure-csv/clojure-csv "2.0.0-alpha1"]
                 [clj-time "0.4.3"]
                 [incanter "1.3.0"]

                 [org.encog/encog-core "3.1.0"]   ; official encog 3.1 release
                 [clojure-encog "0.4.1-SNAPSHOT"]] ; my code
  :plugins [[lein-swank "1.4.4"]]
  :dev-dependencies [[midje "1.4.0"]
                     [lein-midje "1.0.10"]
                     [com.stuartsierra/lazytest "1.2.3"]
                     ]
  :repositories {"stuart" "http://stuartsierra.com/maven2"}

  :resources-path ".:src/:test/:etc/:etc/data/"
)