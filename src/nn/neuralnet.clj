(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce])
)

(defn linear-combiner
  [neuron]

  (println "linear-combiner function CALLED")

  (ccoerce/to-long (:time neuron)) 
)


(defn activation
  "Neuron fires iff X1W1 + X2W2 + X3W3 + ... > T"
  [neuron]

  (println "activation function CALLED")

  (let [combined (linear-combiner neuron)]

    
  )
)


(defn get-time-format []
  (cformat/formatter "dd.MM.yyy HH:mm:ss.SSS")
)

(defn create-input-neuron [time ask bid avolume bvolume]

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


