(ns nn.layers.output
  
  (:require [incanter.core :as incanter]
            [clojure.zip :as zip]
            [nn.layers.layers :as layers]
            [nn.util :as util])
)

;; OUTPUT LAYER
(defn create-output-neuron [hidden-layer]
  { :inputs (reduce #(conj  %1 
                            { :input-id (:id %2) 
                              :weight (rand) 
                              :bias 0 } ) 
                    '() 
                    hidden-layer)
    :id (util/generate-id)
  }
)
(defn create-output-layer [hidden-layer]
  (let [output-layer '()]

    (-> output-layer
        (conj (create-output-neuron hidden-layer))
        ;;(conj (create-output-neuron hidden-layer))
    )
  )
)


;; CALCULATE VALUES
(defn traverse-neural-layer
  "A common way to traverse the :output-layer of the neural network"
  [ilayer neural-layer edit-fn]
  
  (loop [loc (layers/create-zipper neural-layer)]
    
    (if (zip/end? loc)
      (zip/root loc)
      (if (and  (-> loc zip/node map?) 
                (-> loc zip/node (contains? :input-id)))
        (recur  (zip/next
                  (zip/edit loc merge (edit-fn loc ilayer))))
        (recur (zip/next loc))
      )
    )
  )
)

(defn calculate-leaf-value [hidden-layer neural-layer]


      (traverse-neural-layer hidden-layer neural-layer    ;; pass in the output layer
                             (fn [loc hlayer]    ;; pass in the edit fn
                               (let  [ val (:calculted-value (first (filter (fn [ech]
                                                                              (= (:id ech) (:input-id (zip/node loc))) )  ;; lookup value based on input-id (:value (zip/node loc))
                                                                    hlayer)))
                                       wei (:weight (zip/node loc))
                                       calculated (* val wei) ]
                                 { :calculated calculated }))
      )

  
  #_(loop [loc (layers/create-zipper neural-layer)]
    (if (zip/end? loc)
      (zip/root loc)
      (if (and  (-> loc zip/node map?) 
                (-> loc zip/node (contains? :input-id)))
        (recur  (zip/next
                  (zip/edit loc merge
                    
                    
                    (let  [ val (:calculted-value (first (filter (fn [ech]
                                                                    (= (:id ech) (:input-id (zip/node loc))) )  ;; lookup value based on input-id (:value (zip/node loc))
                                                                  hidden-layer)))
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
  (merge ech-map  { :calculted-value (reduce (fn [rst nxt] (+ rst (:calculated nxt))) 
                                              0 
                                              (:inputs ech-map))
                  }
  )
)
(defn calculate-value [hidden-layer neural-layer]
  
  ;; first calculate leaf values, then map calculated-values over the result list
  (map calculate-final-value (calculate-leaf-value hidden-layer neural-layer))
)


;; CALCULATE ERRORS

(defn calculate-leaf-error [neural-layer total-error]

  (loop [loc (layers/create-zipper neural-layer)]
    (if (zip/end? loc)
      (zip/root loc)
      (if (and  (-> loc zip/node map?) 
                (-> loc zip/node (contains? :input-id)))
        (recur  (zip/next
                  (zip/edit loc merge
                    
                    (let  [ wei (:weight (zip/node loc))
                            calculated (* wei total-error) ]
                          { :error calculated })
                  ))) 
        (recur (zip/next loc))
      )
    ) 
  )
)

(defn calculate-error [neural-layer total-error]
  
)


