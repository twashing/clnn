(ns nn.layers.hidden

  (:require [incanter.core :as incanter]
            [clojure.zip :as zip]
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
#_(defn calculate-final-value [ech-map]
  (merge ech-map  { :calculated-value (reduce (fn [rst nxt] (+ rst (:calculated nxt))) 
                                              0 
                                              (:inputs ech-map))
                  }
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
(defn calculate-leaf-error [neural-layer total-error]
  
  (layers/traverse-neural-layer nil neural-layer    ;; pass in i) no dependent layer and ii) the output layer
                         (fn [loc _]         ;; pass in the edit fn
                           
                           (let  [ wei (:weight (zip/node loc))
                                   error (* wei total-error) ]
                             { :error error })
                         )
  )
)
(defn calculate-final-error [ech-map]
  (merge ech-map { :calculated-error (reduce (fn [rst nxt] (+ rst (:error nxt))) 
                                             0 
                                             (:inputs ech-map))})
)
(defn calculate-error [neural-layer total-error]
  (map calculate-final-error (calculate-leaf-error neural-layer total-error))
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

