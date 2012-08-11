(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [incanter.core :as incanter]
            [clojure.pprint :as pprint]
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


;; HIDDEN LAYER
(defn create-hidden-neuron [input-layer]

  {:value 0
   :inputs (reduce #(conj %1 { :key (:key %2) :value (:value %2) :weight (rand) }) '() input-layer)
   
   :bias 0
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
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
        (conj (create-hidden-neuron input-layer))
    )
  )
)


;; Linear Combiner & Activation FUNCTIONS
(defn linear-combiner
  [neuron]
  
  (println (str "linear-combiner function CALLED > " (-> neuron :inputs)))
  (reduce #(+ %1 (* (:value %2)
                    (:weight %2)))
          0
          (:inputs neuron)
  )
  
)
(defn activation
  "Neuron fires iff X1W1 + X2W2 + X3W3 + ... > T"
  [value]
  
  (/ 1 (+ 1 (incanter/exp (* -1 value))))
  
)


;; --- testing 
(require '[nn.config.config :as config])
(defn test-hidden-layer[]

  (let [train-data (config/load-train-data)
        input-layer (create-input-layer (second train-data))
        hidden-layer (create-hidden-layer input-layer)]
    
    (pprint/pprint "Input Layer:")
    (pprint/pprint input-layer)
    
    (pprint/pprint "Hidden Layer:")
    (pprint/pprint hidden-layer)
    ;(map #(neuralnet/linear-combiner %1) hidden-layer)
    
    ;; apply linear combiner and add the bias to get a value
    (comment
    (let [combined (+ (linear-combiner (first hidden-layer))
                      (:bias (first hidden-layer)))
          activation (activation combined)
         ]
      (println (str "Hidden Combined value: " combined))
      (println (str "Hidden Activation value: " activation))
      )
    )
    
    (let [hidden-updated (map (fn [ech]
                                (let [combined (+ (linear-combiner ech) (:bias ech))
                                      pass1 (assoc ech :value-combined combined)
                                      pass2 (assoc pass1 :value-activation (activation combined))]
                                  pass2
                                ))
                              
                              hidden-layer
                         )
         ]
      (println "Hidden Layer , Combined and Activated:")
      (pprint/pprint hidden-updated)
    )
    (comment let [hidden-updated (map println hidden-layer)]
      
    )
  )
)

;;(test-hidden-layer)