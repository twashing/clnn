(ns nn.layers.input
  (:require [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [clojure.zip :as zip]
            [nn.layers.layers :as layers]
            [nn.util :as util]
  )
)



;; NORMALIZE INPUT DATA
(defn normalize-data [multiplier data-double]
  (/ data-double multiplier)
)


;; INPUT LAYER
(defn get-time-format []
  (cformat/formatter "dd.MM.yyy HH:mm:ss.SSS")
)

(defn create-input-neuron [input-map]
  
  (merge input-map { :weight (rand)
                     :id (util/generate-id) })
)
(defn create-input-layer
  "Creating a neuron for each input value"
  [inputs]
  
  (let [input-layer '()
        tformat (get-time-format)
       ]
    [(create-input-neuron { :key :time :value (->> (first inputs) (cformat/parse tformat) ccoerce/to-long double (normalize-data 1000000000000) ) } )
      (create-input-neuron { :key :bid :value (-> (second inputs) Double/parseDouble ) } )
      (create-input-neuron { :key :ask :value (-> (nth inputs 2) Double/parseDouble ) } )
      (create-input-neuron { :key :bvolume :value (->> (nth inputs 3) Double/parseDouble (normalize-data 1000000) ) } )
      (create-input-neuron { :key :avolume :value (->> (nth inputs 4) Double/parseDouble (normalize-data 1000000) ) } )
    ]
  )
)


;; CALCULATE VALUES
(defn calculate-value [neural-layer]
  
  (map (fn [ech]
         (let  [val (:value ech)
                wei (:weight ech)
                calculated (* val wei) ]
           (merge ech { :calculated-value calculated })))
       neural-layer
  )
)


;; CALCULATE ERROR
(defn calculate-error
  "neural-layer is the layer under calculation. error-layer is the previous layer from where we are backpropagating the error value"
  [neural-layer error-layer]
  
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
         
         berror-layer (map (fn [eneuron] (merge eneuron { :backpropagated-error (* (:calculated-value eneuron)
                                                                                   ;;(- 1 (:calculated-value eneuron))
                                                                                   (:calculated-error eneuron))
                                                         }))
                            cerror-layer)
        pderiv-layer (map (fn [eneuron]
                            (merge eneuron
                                   { :inputs (map (fn [ech]
                                                    (merge ech { :partial-derivative (* (:backpropagated-error eneuron)
                                                                                        (:value ech))
                                                               } ))
                                                  (:inputs eneuron))
                                   }
                            )
                          )
                          berror-layer)
                     #_(map (fn [eneuron] (merge eneuron { :partial-derivative (* (:backpropagated-error eneuron)
                                                                                  (:calculated-value eneuron))
                                                         }))
                            berror-layer)
        
        ]
    pderiv-layer
  )
)

