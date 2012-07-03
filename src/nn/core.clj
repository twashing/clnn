(ns nn.core
  
  (:require [clojure.pprint :as pprint]
            [clojure-csv.core :as csv]
            [clojure.java.io :as io]
  )
  
)


;; consume test data (from config)
(let [config (load-file "etc/config.clj")
      dname (-> config :data :test)
     ]

  (pprint/pprint (csv/parse-csv (io/reader dname)))
)


;; create neuron protocol (using jodatime)


;; build input layer


;; build hidden layer (1/2 no. of input neurons)
;; make connection to each neuron in input layer


;; build output layer
;; make connection to each neuron in hidden layer

