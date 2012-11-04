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
(defn create-input-neuron [inputs]
  
  {:inputs (reduce #(conj %1 { :key (:key %2) :value (:value %2) :weight (rand) :bias 0 }) '() inputs)
   :id (util/generate-id)
  }
)

(defn create-input-layer
  "Creating a neuron for each input value"
  [inputs]

  (let [input-layer '()
        tformat (get-time-format)
        input-list [
          { :key :time :value (->> (first inputs) (cformat/parse tformat) ccoerce/to-long double (normalize-data 1000000000000) ) }
          { :key :bid :value (-> (second inputs) Double/parseDouble ) }
          { :key :ask :value (-> (nth inputs 2) Double/parseDouble ) }
          { :key :bvolume :value (->> (nth inputs 3) Double/parseDouble (normalize-data 1000000) ) }
          { :key :avolume :value (->> (nth inputs 4) Double/parseDouble (normalize-data 1000000) ) }
        ]
       ]
    (-> input-layer
        (conj (create-input-neuron input-list ))
        (conj (create-input-neuron input-list ))
        (conj (create-input-neuron input-list ))
        (conj (create-input-neuron input-list ))
        (conj (create-input-neuron input-list ))
    )
  )
)


;; CALCULATE VALUES
(defn calculate-leaf-value [neural-layer]
  
  (loop [loc (layers/create-zipper neural-layer)]
    
    (if (zip/end? loc)
      (zip/root loc)
      (if (and  (-> loc zip/node map?) 
                (-> loc zip/node (contains?   :key)))
        (recur  (zip/next
                  (zip/edit loc merge
                    
                    (let  [ val (:value (zip/node loc))
                            wei (:weight (zip/node loc))
                            calculated (* val wei) ]
                          { :calculated calculated })
                    
                  ))) 
        (recur (zip/next loc))
      )
    ) 
  )
)

(defn calculate-final-value [ech-map]
  (merge ech-map  { :calculated-value (-> ech-map layers/calculate-linear-combiner layers/calculate-activation)
                  }
  )
)
(defn calculate-value [neural-layer]
  
  ;; first calculate leaf values, then map calculated-values over the result list
  (map calculate-final-value (calculate-leaf-value neural-layer))
)


;; CALCULATE ERROR
#_(defn calculate-leaf-error [neural-layer total-error]
  
  (loop [loc (layers/create-zipper neural-layer)]
    
    (if (zip/end? loc)
      (zip/root loc)
      (if (and  (-> loc zip/node map?) 
                (-> loc zip/node (contains?   :key)))
        (recur  (zip/next
                  (zip/edit loc merge
                    
                    (let  [ wei (:weight (zip/node loc))
                            error (* wei total-error) ]
                      { :error error })
                  ))) 
        (recur (zip/next loc))
      )
    ) 
  )
)
#_(defn calculate-final-error [ech-map]
  (merge ech-map { :calculated-error (reduce (fn [rst nxt] (+ rst (:error nxt))) 
                                             0 
                                             (:inputs ech-map))})
)
#_(defn calculate-error [neural-layer total-error]
  (map calculate-final-error (calculate-leaf-error neural-layer total-error))
)
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
                                                                                    (- 1 (:calculated-value eneuron))
                                                                                    (:calculated-error eneuron))
                                                         }))
                            cerror-layer)
         pderiv-layer (map (fn [eneuron] (merge eneuron { :partial-derivative (* (:backpropagated-error eneuron)
                                                                                  (:calculated-value eneuron))
                                                         }))
                            berror-layer)
        
        ]
    pderiv-layer
  )
)

