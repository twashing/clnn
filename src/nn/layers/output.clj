(ns nn.layers.output
  
  (:require [incanter.core :as incanter]
            [nn.util :as util])
)

;; OUTPUT LAYER
(defn create-output-neuron [hidden-layer]
  { :inputs (reduce #(conj  %1 
                            { :value (:value-activation %2) 
                              :input-id (:id %2) 
                              :weight (rand) 
                              :bias 0 } ) 
                    '() 
                    hidden-layer)
    :id (util/generate-id)
  }
)
(defn create-output-layer [hidden-layer]
  (let [output-layer '()]

    (-> output-layer
        (conj (create-output-neuron hidden-layer))
        (conj (create-output-neuron hidden-layer))
    )
  )
)

