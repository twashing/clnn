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


(fact "create input layer"
  (let [train-data (config/load-config)
        input-layer (neuralnet/create-input-layer (second train-data))
       ]
    ;(pprint/pprint input-layer)
    (-> input-layer nil? not) => true
    (-> input-layer empty? not) => true
  )
)


(fact "test the linear combiner function"

      (let [neuron
            {:time 1335902400676   ; long value of #<DateTime 2012-05-01T20:00:00.676Z>,
             :bid "1.32379",
             :ask "1.32390",
             :bvolumne "2250000.00",
             :avolume "3000000.00",
             :threshold 0.76,
             :weights {:time 0.90,
                       :bid 0.68,
                       :ask 0.64,
                       :bvolume 0.48,
                       :avolume 0.58}}
            ])
)

; create hidden layer

; create output layer
