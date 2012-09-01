(ns nn.layers.input
  (:require [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
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
  
  #_{:key key
   :value value
   :weight (rand)
  }
  {:inputs (reduce #(conj %1 { :key (:key %2) :value (:value %2) :weight (rand) :bias 0 }) '() inputs)
   :id (util/generate-id)
  }
)

(defn create-input-layer
  "Creating a neuron for each input value"
  [inputs]

  (let [input-layer '()
        tformat (get-time-format)
        input-list '(
          { :key :time :value (->> (first inputs) (cformat/parse tformat) ccoerce/to-long double (normalize-data 1000000000000) ) }
          { :key :bid :value (-> (second inputs) Double/parseDouble ) }
          { :key :ask :value (-> (nth inputs 2) Double/parseDouble ) }
          { :key :bvolume :value (->> (nth inputs 3) Double/parseDouble (normalize-data 1000000) ) }
          { :key :avolume :value (->> (nth inputs 4) Double/parseDouble (normalize-data 1000000) ) }
        )
       ]
    (-> input-layer
        (conj (create-input-neuron :time input-list ))
        (conj (create-input-neuron :bid input-list ))
        (conj (create-input-neuron :ask input-list ))
        (conj (create-input-neuron :bvolume input-list ))
        (conj (create-input-neuron :avolume input-list ))
    )
  )
)
