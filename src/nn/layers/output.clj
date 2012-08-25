(ns nn.layers.output

  
)

;; OUTPUT LAYER
(defn create-output-neuron [hidden-layer]
  {:inputs (reduce #(conj %1 { :value (:value-activation %2) :hidden-neuron-id (:id %2) :weight (rand) :bias 0 } ) '() hidden-layer)
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

