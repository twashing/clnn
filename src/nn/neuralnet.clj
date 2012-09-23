(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [incanter.core :as incanter]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [nn.layers.input :as ilayer]
            [nn.layers.hidden :as hlayer]
            [nn.layers.output :as olayer]
            [nn.config.config :as config]
            [nn.util :as util]
  )
)


;; --- testing 
(defn test-hidden-layer[]

  (let [train-data (config/load-train-data)
        input-layer (ilayer/create-input-layer (second train-data))
        hidden-layer (hlayer/create-hidden-layer input-layer)]
    
    (pprint/pprint "Input Layer:")
    (pprint/pprint input-layer)
    
    (pprint/pprint "Hidden Layer:")
    (pprint/pprint hidden-layer)
    
    ;; apply linear combiner and add the bias to get a value
    (let [hidden-updated (hlayer/apply-combiner-activation hidden-layer)]
      
      (println "Hidden Layer , Combined and Activated:")
      (pprint/pprint hidden-updated)
    )
  )
)

(defn test-output-layer[]

  (let [train-data (config/load-train-data)
        input-layer (ilayer/create-input-layer (second train-data))
        hidden-layer (hlayer/create-hidden-layer input-layer)
        hidden-updated (hlayer/apply-combiner-activation hidden-layer)
        output-layer (olayer/create-output-layer hidden-updated)
       ]
    (pprint/pprint "Output Layer:")
    (pprint/pprint output-layer)
  )
)


;; --- propogation 
(defn update-weights []  
  ;; ...
)


(defn create-neural-network [single-tick-data]
  
  (let [
        input-layer (ilayer/create-input-layer single-tick-data)
        hidden-layer (hlayer/create-hidden-layer input-layer)        
        output-layer (olayer/create-output-layer hidden-layer)
       ]
    {:input-layer input-layer
     :hidden-layer hidden-layer
     :output-layer output-layer
    }
  )
)


;; referencing this page: http://galaxy.agh.edu.pl/~vlsi/AI/backp_t_en/backprop.html
(defn feed-forward [neural-network]
  
  ;; propagate price signal (start with bid) through the network
  (let  [ nni (ilayer/calculate-value (:input-layer neural-network))
          nnh (hlayer/calculate-value nni (:hidden-layer neural-network))
          nno nil ;;(olayer/calculate-value nnh (:output-layer neural-network))
        ]    
    {:input-layer nni
     :hidden-layer nnh
     :output-layer nno
    }
  )
)
(defn calculate-total-error [nlayer next-tick]
  
  ;; in output neurons, calculate error between output (start with bid) and actual bid
  (let [calculated-ask (-> nlayer first :calculted-value)
        actual-ask (-> next-tick second Double/parseDouble)
        ask-error (- calculated-ask actual-ask)
        ]
    ask-error
  )
)
(defn propogate-error [neural-network]
  
  ;; 
  (let [nei (-> neural-network :input-layer ilayer/calculate-error)
        ;;neh (-> neural-network :input-layer hlayer/calculate-error)
        ;;neo (-> neural-network :input-layer olayer/calculate-error)
       ]
    
  )
)



;;(pprint/pprint results)

;; apply total error to weight in each neuron -> going backwards through neuralnet 

;; apply weight change using ... partial derivative of the weighted error... -> going forwards through the neuralnet



(use 'clojure.stacktrace)


(defn thing []

    (def train-data (config/load-train-data))
    (def first-tick (second train-data))
    (def neural-network (create-neural-network first-tick))
    
    (def iteration-history (conj [] { :tick-data first-tick :neural-network neural-network }))
    (def next-tick (nth train-data 2))
    
    (def nn (feed-forward neural-network))
    
    (def terror (calculate-total-error (:output-layer nn) next-tick))

    (pprint/pprint nn)
    (pprint/pprint neural-network)
    
    (def nni (ilayer/calculate-value (:input-layer neural-network)))
    (def nnh (hlayer/calculate-leaf-value nni (:hidden-layer neural-network)))
    
    
    ;; run 1 iteration... see results
    (def nn (feed-forward neural-network next-tick))
    (def hist (conj iteration-history { :tick-data next-tick :neural-network nn }))
    
    
    ;; train until an acceptable margin of error
    
)

;; by default this i) takes the train data and 2) combiner and activation functions on hidden layer

;; function > predict bid (1 iteration based on a single piece of tick data)

;; structure > to store previous tick data values

;; predict bid & ask


