(defproject clnn "1.0.0-SNAPSHOT"
  :description "This is me dablling in Neural Networks; Trying to build a predicotr for financial time series-"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-csv/clojure-csv "2.0.2"]
                 [clj-time "0.12.0"]
                 [incanter "1.9.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 ;;[org.encog/encog-core "3.1.0"]   ;; official encog 3.1 release
                 ;;[clojure-encog "0.4.1-SNAPSHOT"]
                 
                 ]
  :dev-dependencies [[midje "1.4.0"]
                     [lein-midje "1.0.10"]
                     [com.stuartsierra/lazytest "1.2.3"]]
  :repositories {"stuart" "http://stuartsierra.com/maven2"}

  :resources-path ".:src/:test/:etc/:etc/data/"

  :cljfmt {:indents ^:replace {#".*" [[:inner 0]]}})
