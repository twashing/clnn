(ns nn.layers.hidden

  (:require [incanter.core :as incanter]
            [nn.util :as util]
            )
)

;; HIDDEN LAYER
(defn create-hidden-neuron [input-layer]
  
  {:inputs (reduce #(conj %1 { :key (:key %2) :value (:value %2) :weight (rand) :bias 0 }) '() input-layer)
   :id (util/generate-id)
  }
)
(defn create-hidden-layer [input-layer]
  (let [hidden-layer '()]
    
    (-> hidden-layer
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
    )
  )
)


;; Linear Combiner & Activation FUNCTIONS
(defn linear-combiner
  [neuron]
  
  ;;(println (str "linear-combiner function CALLED > " (-> neuron :inputs)))
  (reduce (fn [rlt ech]
            (+ rlt (+ (* (:value ech)
                         (:weight ech))
                      (:bias ech))) )
          0
          (:inputs neuron)
  )
)

(defn activation
  "Neuron fires iff X1W1 + X2W2 + X3W3 + ... > T"
  [value]
  
  (/ 1 (+ 1 (incanter/exp (* -1 value))))
)

(defn apply-combiner-activation [hidden-layer]
  (map (fn [ech]
         (let [combined (linear-combiner ech)
               pass1 (assoc ech :value-combined combined)
               pass2 (assoc pass1 :value-activation (activation combined))
              ]
           pass2
         )
       )
       hidden-layer
  )
)

