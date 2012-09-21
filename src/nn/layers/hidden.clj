(ns nn.layers.hidden

  (:require [incanter.core :as incanter]
            [clojure.zip :as zip]
            [nn.util :as util])
)

;; HIDDEN LAYER
(defn create-hidden-neuron [input-layer]
  
  {:inputs (reduce  #(conj  %1 
                            { :input-id (:id %2) 
                              :weight (rand) 
                              :bias 0 }) 
                    '() 
                    input-layer)
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

;; CALCULATE VALUES
(defn calculate-leaf-value [input-layer neural-layer]
  
  (loop [loc (zip/zipper  (fn [node]
                            (or (map? node)
                                (list? node)))
                          (fn [node]
                            (cond 
                              (nil? node)   nil
                              (map? node)   (:inputs node)
                              :else         node))
                          (fn [node children]
                            (cond
                              (nil? node)   nil
                              (map? node)   (assoc node :inputs children)
                              (list? node)  (into '() children)
                              :else       node))
                          neural-layer)]
    
    (if (zip/end? loc)
      (zip/root loc)
      (if (and  (-> loc zip/node map?) 
                (-> loc zip/node (contains? :input-id)))
        (do
        (recur  (zip/next
                  (zip/edit loc merge
                    
                    (let  [ val 0 ;; lookup value based on input-id (:value (zip/node loc))
                            wei (:weight (zip/node loc))
                            calculated (* val wei) ]
                          { :calculated calculated })
                  ))) 
        )
        (recur (zip/next loc))
      )
    ) 
  )
)
(defn calculate-final-value [ech-map]
  (merge ech-map  { :calcualted-value (reduce (fn [rst nxt] (+ rst (:calculated nxt))) 
                                              0 
                                              (:inputs ech-map))
                  }
  )
)
(defn calculate-value [input-layer neural-layer]
  
  ;; first calculate leaf values, then map calculated-values over the result list
  (map calculate-final-value (calculate-leaf-value input-layer neural-layer))
)



;; Linear Combiner & Activation FUNCTIONS
(defn linear-combiner
  [neuron]
  
  ;;(println (str "linear-combiner function CALLED > " (-> neuron :inputs)))
  (reduce (fn [rlt ech]
            (+ rlt (+ (* (:value ech)
                         (:weight ech))
                      (:bias ech))) )
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
         (let [combined (linear-combiner ech)
               pass1 (assoc ech :value-combined combined)
               pass2 (assoc pass1 :value-activation (activation combined))
              ]
           pass2
         )
       )
       hidden-layer
  )
)

