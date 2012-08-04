(ns nn.encog

  )

(def network      ;;def-ing it here for demo purposes
  (make-network {:input   32
                 :output  1
                 :hidden [50 10]} ;; 2 hidden layers
                (make-activationF :sigmoid)
                (make-pattern :feed-forward)))