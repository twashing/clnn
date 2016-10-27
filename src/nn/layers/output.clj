(ns nn.layers.output

  (:require [incanter.core :as incanter]
    [clojure.zip :as zip]
    [clojure.pprint :as pprint]
    [nn.layers.layers :as layers]
    [nn.util :as util]))

;; OUTPUT LAYER
(defn create-output-neuron [hidden-layer]
  {:inputs (reduce #(conj  %1
                      {:input-id (:id %2)
                       :weight (rand)})
             '()
             hidden-layer)
   :id (util/generate-id)})
(defn create-output-layer [hidden-layer]
  (let [output-layer '()]

    (-> output-layer
      (conj (create-output-neuron hidden-layer))
        ;;(conj (create-output-neuron hidden-layer))
)))

;; CALCULATE VALUES
(defn calculate-leaf-value [hidden-layer neural-layer]

  (layers/traverse-neural-layer hidden-layer neural-layer    ;; pass in the output layer
    (fn [loc hlayer]             ;; pass in the edit fn

      (let  [val (:calculated-value (first (filter (fn [ech]
                                                     (= (:id ech) (:input-id (zip/node loc))))  ;; lookup value based on input-id (:value (zip/node loc))
                                             hlayer)))
             wei (:weight (zip/node loc))
             calculated (* val wei)]
        {:calculated calculated}))))
(defn calculate-final-value [ech-map]
  (merge ech-map  {:calculated-value (+ 1 (-> ech-map layers/calculate-linear-combiner layers/calculate-activation))}))
(defn calculate-value [hidden-layer neural-layer]

  ;; first calculate leaf values, then map calculated-values over the result list
  (map calculate-final-value (calculate-leaf-value hidden-layer neural-layer)))

;; CALCULATE ERRORS
(defn calculate-leaf-error [neural-layer total-error]

  (layers/traverse-neural-layer nil neural-layer    ;; pass in i) no dependent layer and ii) the output layer
    (fn [loc _]         ;; pass in the edit fn

      (let  [wei (:weight (zip/node loc))
             error (* wei total-error)]
        {:error error}))))

(defn calculate-calculated-error [ech-map]

  (merge ech-map {:calculated-error (reduce (fn [rst nxt] (+ rst (:error nxt)))
                                      0
                                      (:inputs ech-map))}))
(defn calculate-error [input-layer neural-layer total-error]

  ;; take :calculated-error A)
  ;; backpropagated error   B) is:                  :calculated-value * ( 1 - :calculated-value ) * A)
  ;; partial derivative     C) is:                  B) * :calculated-value

  ;; weight change          D) is:
  ;;    - theta (learning const) * partial derivative

  ;; for hidden layer... pass in input error from connecting neuron (do not use "total error")
  (let [cerror-layer (map (fn [eneuron] (merge eneuron {:calculated-error total-error})) neural-layer)
        berror-layer (map (fn [eneuron] (merge eneuron {:backpropagated-error (* (:calculated-value eneuron)
                                                                                   ;;(- 1 (:calculated-value eneuron))
                                                                                (- (:calculated-value eneuron) total-error))}))
                       cerror-layer)

        pderiv-layer (map (fn [eneuron]
                            (merge eneuron
                              {:inputs (map (fn [ech]
                                              (merge ech {:partial-derivative (* (:backpropagated-error eneuron)
                                                                                (:calculated-value (first (filter #(= (:id %1) (:input-id ech)) input-layer))))}))
                                         (:inputs eneuron))}))
                       berror-layer)]
    pderiv-layer))

