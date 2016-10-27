(ns nn.test.core
  (:use [clojure.test]
        [midje.sweet])
  (:require [nn.core :as nn]
            [nn.config.config :as config]
            [nn.neuralnet :as neuralnet]
            [nn.layers.input :as ilayer]
            [nn.layers.hidden :as hlayer]
            [clj-time.format :as cformat]
            [clojure.pprint :as pprint]))

(fact "load config training file; ensure first tickis as expected"
  (-> (config/load-train-data) nil? not) => true)

(fact "we can get the next tick"
  (let [train-data (config/load-train-data)
        next-tick (nn/next-tick train-data)]
    (-> next-tick nil? not) => true))

(fact "create input layer"
  (let [train-data (config/load-train-data)
        input-layer (ilayer/create-input-layer (second train-data))]
    ;;(pprint/pprint input-layer)
    (-> input-layer nil? not) => true
    (-> input-layer empty? not) => true))

; (neuralnet/linear-combiner neuron) => 1.2023149806101477E12
(def neuron {:time (cformat/parse (ilayer/get-time-format) "01.05.2012 20:00:00.676")
             :bid 1.32379,
             :ask 1.32390,
             :bvolume 2250000.00,
             :avolume 3000000.00,
             :threshold 0.76,
             :weights {:time 0.90,
                       :bid 0.68,
                       :ask 0.64,
                       :bvolume 0.48,
                       :avolume 0.58}})

#_(fact "test the activation (sigmoid) function"
    (neuralnet/activation neuron) => 1.0)

(fact "create hidden layer"
  (let [train-data (config/load-train-data)
        input-layer (ilayer/create-input-layer (second train-data))
        hidden-layer (hlayer/create-hidden-layer input-layer)]

    (println "Showing Hidden Layer")
    (pprint/pprint hidden-layer)

    1 => 1))

(fact "test the linear combiner function; I expect to see these output values
        
        :time 1335902400676 * 0.90 => 1202312160608.4; 1335902400676  is the long value of #<DateTime 2012-05-01T20:00:00.676Z>
        :bid 1.32379 * 0.68 => 0.9001772
        :ask 1.32390 * 0.64 => 0.847296
        :bvolume 2250000.00 * 0.48 => 1080000
        :avolume 3000000.00 * 0.58 => 1740000

        FINAL => 1202314980610.1474732 (1'202'314'980'610.1474732)"

  (let [train-data #spy/d (config/load-train-data)
        input-layer (ilayer/create-input-layer (second train-data))
        hidden-layer (hlayer/create-hidden-layer input-layer)]

    (let [value (+ (hlayer/linear-combiner (first hidden-layer))
                  (:bias (first hidden-layer)))]

      (println (str "Hidden Neuron value: " value))
      (println (str "Hidden Neuron activation-value: " (hlayer/activation value))))
    
    (= 1 1) => true

    ))

