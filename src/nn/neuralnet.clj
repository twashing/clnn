(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat])
)

(defn linear-combiner [neuron] (println "linear-combiner function CALLED"))
(defn activation [neuron] (println "activation function CALLED"))


(defn get-time-format []
  (cformat/formatter "dd.MM.yyy HH:mm:ss.SSS")
)

(defn create-input-neuron [time ask bid avolume bvolume]

  {:time time
   :bid bid
   :ask ask
   :bvolumne bvolume
   :avolume avolume
   :weights {:time (rand)
             :bid (rand)
             :ask (rand)
             :bvolume (rand)
             :avolume (rand)
            }
  }
)
(defn create-input-layer
  "Creating a neuraon for each input value"
  [inputs]

  (let [input-layer '()
        tformat (get-time-format)
       ]
    (reduce 
       (fn [result next]
         (conj result (create-input-neuron
                            (cformat/parse tformat (first inputs))
                            (second inputs)
                            (nth inputs 2)
                            (nth inputs 3)
                            (nth inputs 4))))
       input-layer
       inputs
    )
  )
)


