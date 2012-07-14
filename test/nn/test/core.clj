(ns nn.test.core
  (:use [clojure.test]
        [midje.sweet]
  )
  (:require [nn.core :as nn]
            [nn.config.config :as config]
            [nn.neuralnet :as neuralnet]
            [clojure.pprint :as pprint]
  )
)



(fact "load config training file; ensure first tickis as expected"
  (-> (config/load-config) nil? not) => true
)


(fact "we can get the next tick"
  (let [train-data (config/load-config)
        next-tick (nn/next-tick train-data)]
    (-> next-tick nil? not) => true
    
  )
)


; create neuron protocol using jodatime

;(fact "create input layer"
  (let [train-data (config/load-config)
        input-layer (neuralnet/create-input-layer (second train-data))
       ]
    (pprint/pprint input-layer)
  )
;)

; create hidden layer

 ; create output layer
