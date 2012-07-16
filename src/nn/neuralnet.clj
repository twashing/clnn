(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [incanter.core :as incanter])
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


(defn get-time-format []
  (cformat/formatter "dd.MM.yyy HH:mm:ss.SSS")
)

(defn create-hidden-neuron [time ask bid avolume bvolume]

  {:time time
   :bid bid
   :ask ask
   :bvolumne bvolume
   :avolume avolume

   :threshold (rand)
   :weights {:time (rand)
             :bid (rand)
             :ask (rand)
             :bvolume (rand)
             :avolume (rand)
            }
  }
)
(defn create-input-neuron [key value]

  {key value
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

