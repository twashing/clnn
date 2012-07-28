(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [incanter.core :as incanter])
)


;; INPUT LAYER
(defn get-time-format []
  (cformat/formatter "dd.MM.yyy HH:mm:ss.SSS")
)
(defn create-input-neuron [key value]

  {:key key
   :value value

   :weight (rand)
   :bias 0
  }
)
(defn create-input-layer
  "Creating a neuron for each input value"
  [inputs]

  (let [input-layer '()
        tformat (get-time-format)
       ]
    (-> input-layer
        (conj (create-input-neuron :time (cformat/parse tformat (first inputs))))
        (conj (create-input-neuron :bid (Double/parseDouble (second inputs))))
        (conj (create-input-neuron :ask (Double/parseDouble (nth inputs 2))))
        (conj (create-input-neuron :bvolume (Double/parseDouble (nth inputs 3))))
        (conj (create-input-neuron :avolume (Double/parseDouble (nth inputs 4))))
    )
  )
)


;; HIDDEN LAYER
(defn create-hidden-neuron [input-layer]

  {:value 0
   :inputs (reduce #(conj %1 { :key (:key %2) :value (:value %2) :weight (rand) }) '() input-layer)
   
   :weight (rand)
   :bias 0
  }
)
(defn create-hidden-layer [input-layer];
  (let [hidden-layer '()]

    (-> hidden-layer
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
    )
  )
)
(defn linear-combiner
  [neuron]

  (println (str "linear-combiner function CALLED > " neuron))

  (+ (* (-> neuron :weights :time) (ccoerce/to-long (:time neuron)))
     (* (-> neuron :weights :bid) (:bid neuron))
     (* (-> neuron :weights :ask) (:ask neuron))
     (* (-> neuron :weights :bvolume) (:bvolume neuron))
     (* (-> neuron :weights :avolume) (:avolume neuron))
     )
  
)

(defn activation
  "Neuron fires iff X1W1 + X2W2 + X3W3 + ... > T"
  [neuron]

  (println "activation function CALLED")

  (let [combined (linear-combiner neuron)]

    (/ 1 (+ 1 (incanter/exp (* -1 combined))))
  )
)
