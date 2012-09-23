(ns nn.layers.layers
  (:require [clojure.zip :as zip]
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
