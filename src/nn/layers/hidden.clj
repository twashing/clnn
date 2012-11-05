(ns nn.layers.hidden

  (:require [incanter.core :as incanter]
            [clojure.zip :as zip]
            [clojure.pprint :as pprint]
            [nn.layers.layers :as layers]
            [nn.util :as util])
)

;; HIDDEN LAYER
(defn create-hidden-neuron [input-layer]
  
  {:inputs (reduce  #(conj  %1 
                            { :input-id (:id %2) 
                              :weight (rand) 
                              :bias 0 }) 
                    '() 
                    input-layer)
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
        ;;(conj (create-hidden-neuron input-layer))
        ;;(conj (create-hidden-neuron input-layer))
        ;;(conj (create-hidden-neuron input-layer))
        ;;(conj (create-hidden-neuron input-layer))
        ;;(conj (create-hidden-neuron input-layer))
    )
  )
)



;; CALCULATE VALUES
(defn calculate-leaf-value [dependent-layer neural-layer]

  (layers/traverse-neural-layer dependent-layer neural-layer    ;; pass in the output layer
                         (fn [loc hlayer]             ;; pass in the edit fn

                           (let  [ val (:calculated-value (first (filter (fn [ech]
                                                                    (= (:id ech) (:input-id (zip/node loc))) )  ;; lookup value based on input-id (:value (zip/node loc))
                                                                 dependent-layer)))
                                   wei (:weight (zip/node loc))
                                   calculated (* val wei) ]
                             { :calculated calculated }) )
  )
)
(defn calculate-final-value [ech-map]
  (merge ech-map  { :calculated-value (-> ech-map layers/calculate-linear-combiner layers/calculate-activation)
                  }
  )
)
(defn calculate-value [input-layer neural-layer]
  
  ;; first calculate leaf values, then map calculated-values over the result list
  (map calculate-final-value (calculate-leaf-value input-layer neural-layer))
)



;; CALCULATE ERRORS
(defn calculate-error
  "neural-layer is the layer under calculation. error-layer is the previous layer from where we are backpropagating the error value"
  [input-layer neural-layer error-layer]
  
  (let [ ;; for each neuron, find the 'input' in the error layer and multiply this weight by elayer's error value
         ;; calculated-error = weight * error + ...
         
         cerror-layer  (layers/traverse-neurons error-layer neural-layer (fn [loc dep-layer]                           ;; 1. traverse neural-layer
                                                                                
                                                                                (let [ iid     (:id (zip/node loc))    ;; 2. for each neuron, find matching input in dep-layer
                                                                                       ilist   (flatten       ;; 3. build a list of { :weight 0.7879318625211504 :input-error -0.39493030034305143 }
                                                                                                 (reduce
                                                                                                   (fn [rst inp]
                                                                                                     (conj rst 
                                                                                                           (merge
                                                                                                            { :input-error (:backpropagated-error inp) }
                                                                                                            (first (filter (fn [x]
                                                                                                                             (= iid (:input-id x)))
                                                                                                                           (:inputs inp)))
                                                                                                           )
                                                                                                     )
                                                                                                   )
                                                                                                   '()
                                                                                                   dep-layer
                                                                                                 )
                                                                                               )
                                                                                    ]
                                                                                  
                                                                                  {:calculated-error         ;; 4. multilpy error & weight for each connection, then return the sum 
                                                                                    (reduce (fn [rst inp]
                                                                                              (+ rst
                                                                                                 (* (:input-error inp)
                                                                                                    (:weight inp)) ) )
                                                                                            0
                                                                                            ilist)
                                                                                 }
                                                                               )
                                                                         )
                       )
         
         berror-layer (map (fn [eneuron] (merge eneuron { :backpropagated-error  (* (:calculated-value eneuron)
                                                                                    ;;(- 1 (:calculated-value eneuron))
                                                                                    (:calculated-error eneuron))
                                                         }))
                            cerror-layer)
        pderiv-layer (map (fn [eneuron]
                            (merge eneuron
                                   { :inputs (map (fn [ech]
                                                    (merge ech { :partial-derivative (* (:backpropagated-error eneuron)
                                                                                        (:calculated-value (first (filter #(= (:id %1) (:input-id ech)) input-layer))))
                                                               }))
                                                  (:inputs eneuron))
                                   }
                            )
                          )
                          berror-layer)        
        ]
    pderiv-layer
  )
)

;; Linear Combiner & Activation FUNCTIONS
(defn linear-combiner
  [neuron]
  
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
  
  (/ 1 (+ 1 (incanter/exp (* -2 value))))
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

