(ns nn.test.core
  (:use [clojure.test]
        [midje.sweet]
  )
  (:require [nn.core :as nn]
            [nn.config.config :as config]
  )
)



(fact "load config training file; ensure first tickis as expected"
  (-> (config/load-config) nil? not) => true
)


#_(let [train-data (config/load-config)]
  (nn/next-tick train-data)
)

#_(fact 
 (let [train-data (config/load-config)]
   
 )
 => truthy
)


; create neuron protocol using jodatime

; create input layer

; create hidden layer

 ; create output layer
