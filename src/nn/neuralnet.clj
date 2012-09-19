(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [incanter.core :as incanter]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
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

(defn trigger-neurons [neural-network]

  ;; set value (for bid)

  ;; trigger activation
)
(defn calculate-total-error []

  ;; ... 
)
(defn propogate-error []

  ;; ... 
)
(defn update-weights []

  ;; ...
  
)


;; referencing this page: http://galaxy.agh.edu.pl/~vlsi/AI/backp_t_en/backprop.html
(defn propogation-resilient2 [neural-network next-tick]
  
  (comment walk/postwalk  (fn [ech] 
                    (if  (and (map? ech) (:value ech))   ;; check that i. we are dealing with a map and ii. that there's a value
                        (let  [ val (:value ech)
                                wei (:weight ech)
                                calculated (* val wei) ]
                              (println "visiting:" (conj ech { :calculated calculated })) 
                              (conj ech { :calculated calculated })
                        )
                    )
                    ech
                  )
                  neural-network
  )
    
    (loop [loc (zip/zipper  map? 
                            #(:inputs %1) 
                            #(assoc %1 :inputs (into '() %2)) 
                            (:input-layer neural-network))] ;; for '(into [] %2)', putting :content list into a vector
      
      (if (zip/end? loc)
        (zip/root loc)
        (if (contains? (zip/node loc) :_id) 
            (recur  (zip/next
                      (zip/edit loc merge 
                        (comment { :_id
                          (.toString (get (zip/node loc) :_id)) ;; gets the org.bson.types.ObjectId, and extract ID String
                        })
                        
                        (let  [ val (:value loc)
                                wei (:weight loc)
                                calculated (* val wei) ]
                              (println "zipping:" (conj loc { :calculated calculated })) 
                              { :calculated calculated }
                        )
                        ))) 
            (recur (zip/next loc))
        )   
      )   
    )
)

(defn propogation-resilient [neural-network next-tick]
  
  ;; propagate price signal (start with bid) through the network
  (defn calculate-value-input [input-list]
    
    (map  #(let  [ val (:value %1)
                  wei (:weight %1)
                  calculated (* val wei) ]
                ;;(pprint/pprint calculated)
                (conj %1 { :calculated calculated })
          )
          input-list
    )
  )
  ;;(def results 
    (-> neural-network :input-layer :inputs (calculate-value-input))
  ;;)

  ;;(pprint/pprint results)
  
  ;; in output neurons, calculate error between output (start with bid) and actual bid
  
  ;; apply total error to weight in each neuron -> going backwards through neuralnet 
  
  ;; apply weight change using ... partial derivative of the weighted error... -> going forwards through the neuralnet
  
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

(defn thing []
  
  (let [train-data (config/load-train-data)
        first-tick (second train-data)
        neural-network (create-neural-network first-tick)
        
        iteration-history (conj [] { :tick-data first-tick :neural-network neural-network }) ;; record tick & neuraln result
        next-tick (nth train-data 2)
       ]
    
    (pprint/pprint neural-network)
    
    ;; run 1 iteration... see results
    (def nn (propogation-resilient neural-network next-tick))
    (def hist (conj iteration-history { :tick-data next-tick :neural-network nn }))
    
    
    ;; train until an acceptable margin of error
    
  )
)


;; by default this i) takes the train data and 2) combiner and activation functions on hidden layer

;; function > predict bid (1 iteration based on a single piece of tick data)

;; structure > to store previous tick data values

;; predict bid & ask


