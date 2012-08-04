(ns nn.encog
  (:require [clojure-encog.nnets :as enets])
)

;; 1. create a neural network 
(def network      ;;def-ing it here for demo purposes
  (enets/make-network {:input   32
                       :output  1
                       :hidden [50 10] } ;; 2 hidden layers
                      
                      (enets/make-activationF :sigmoid)
                      (enets/make-pattern :feed-forward)))

;; 2. train the neural network

;; 3. run the neural network

