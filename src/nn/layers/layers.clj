(ns nn.layers.layers
  (:require [clojure.zip :as zip]
            [incanter.core :as incanter]
  )
)

(defn create-zipper [neural-layer]

  (zip/zipper  (fn [node]
                 (or (map? node)
                     (list? node)
                     (seq? node)))
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
                  (seq? node)   (into '() children)
                  :else       node))
               neural-layer)
)


(defn calculate-activation
  "Common activation function"
  [value]
  
  (/ 1 (+ 1 (incanter/exp (* -1 value))))
)

;; CALCULATE ERRORS
(defn traverse-neural-layer
  "A common way to traverse the :output-layer of the neural network"
  [dependent-layer neural-layer edit-fn]

  { :pre [(not (nil? neural-layer))
          (or (list? neural-layer)
              (seq? neural-layer))
         ]
  }
  
  (loop [loc (create-zipper neural-layer)]
    
    (if (zip/end? loc)
      (zip/root loc)
      (if (and  (-> loc zip/node map?) 
                (-> loc zip/node (contains? :input-id)))
        (recur  (zip/next
                 (zip/edit loc merge (edit-fn loc dependent-layer))))
        (recur (zip/next loc))
      )
    )
  )
)

