(ns nn.layers.input
  (:require [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
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
(defn create-input-neuron [key value]
  {:key key
   :value value
  }
)

(defn create-input-layer
  "Creating a neuron for each input value"
  [inputs]

  (let [input-layer '()
        tformat (get-time-format)
       ]
    (-> input-layer
        (conj (create-input-neuron :time (->> (first inputs) (cformat/parse tformat) ccoerce/to-long double (normalize-data 1000000000000) )))
        (conj (create-input-neuron :bid (-> (second inputs) Double/parseDouble )))
        (conj (create-input-neuron :ask (-> (nth inputs 2) Double/parseDouble )))
        (conj (create-input-neuron :bvolume (->> (nth inputs 3) Double/parseDouble (normalize-data 1000000) )))
        (conj (create-input-neuron :avolume (->> (nth inputs 4) Double/parseDouble (normalize-data 1000000) )))
    )
  )
)
