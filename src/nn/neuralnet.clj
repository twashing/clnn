(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [incanter.core :as incanter]
            [clojure.pprint :as pprint]
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

  {:inputs (reduce #(conj %1 { :key (:key %2) :value (:value %2) :weight (rand) }) '() input-layer)
   :bias 0
   :id (util/generate-id)
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
  
  ;;(println (str "linear-combiner function CALLED > " (-> neuron :inputs)))
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
(defn apply-combiner-activation [hidden-layer]
  (map (fn [ech]
         (let [combined (+ (linear-combiner ech) (:bias ech))
               pass1 (assoc ech :value-combined combined)
               pass2 (assoc pass1 :value-activation (activation combined))]
           pass2
       ))
       hidden-layer
  )
)

;; OUTPUT LAYER
(defn create-output-neuron [hidden-layer]
  {:inputs (reduce #(conj %1 { :value (:value-activation %2) :hidden-neuron-id (:id %2) :weight (rand) }) '() hidden-layer)
  }
)
(defn create-output-layer [hidden-layer]
  (let [output-layer '()]

    (-> output-layer
        (conj (create-output-neuron hidden-layer))
        (conj (create-output-neuron hidden-layer))
    )
  )
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

(defn test-output-layer[]

  (let [train-data (config/load-train-data)
        input-layer (create-input-layer (second train-data))
        hidden-layer (create-hidden-layer input-layer)
        hidden-updated (apply-combiner-activation hidden-layer)
        output-layer (create-output-layer hidden-updated)
       ]
    (pprint/pprint "Output Layer:")
    (pprint/pprint output-layer)
  )
)

