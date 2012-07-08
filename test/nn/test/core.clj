(ns nn.test.core
  (:use [nn.core]
        [clojure.test]
        [midje.sweet]
  )
  (:require [nn.config.config :as config]
  )

)



(fact "load config training file; ensure first tickis as expected"
      (config/load-config) => truthy

)


; create neuron protocol using jodatime

; create input layer

; create hidden layer

 ; create output layer
