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
  (merge ech-map  { :calculated-value (reduce (fn [rst nxt] (+ rst (:calculated nxt))) 
                                              0 
                                              (:inputs ech-map))
                  }
  )
)
(defn calculate-value [neural-layer]
  
  ;; first calculate leaf values, then map calculated-values over the result list
  (map calculate-final-value (calculate-leaf-value neural-layer))
)


;; CALCULATE ERROR
(defn calculate-error [neural-layer]

  
)


