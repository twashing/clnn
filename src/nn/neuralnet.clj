(ns nn.neuralnet
  (:require [clj-time.core :as ctime]
            [clj-time.format :as cformat]
            [clj-time.coerce :as ccoerce]
            [incanter.core :as incanter]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
            [nn.layers.layers :as layers]
            [nn.layers.input :as ilayer]
            [nn.layers.hidden :as hlayer]
            [nn.layers.output :as olayer]
            [nn.config.config :as config]
            [nn.util :as util]
  )
)


;; --- testing 
(defn test-hidden-layer[]

  (let [train-data (config/load-train-data)
        input-layer (ilayer/create-input-layer (second train-data))
        hidden-layer (hlayer/create-hidden-layer input-layer)]
    
    (pprint/pprint "Input Layer:")
    (pprint/pprint input-layer)
    
    (pprint/pprint "Hidden Layer:")
    (pprint/pprint hidden-layer)
    
    ;; apply linear combiner and add the bias to get a value
    (let [hidden-updated (hlayer/apply-combiner-activation hidden-layer)]
      
      (println "Hidden Layer , Combined and Activated:")
      (pprint/pprint hidden-updated)
    )
  )
)

(defn test-output-layer[]

  (let [train-data (config/load-train-data)
        input-layer (ilayer/create-input-layer (second train-data))
        hidden-layer (hlayer/create-hidden-layer input-layer)
        hidden-updated (hlayer/apply-combiner-activation hidden-layer)
        output-layer (olayer/create-output-layer hidden-updated)
       ]
    (pprint/pprint "Output Layer:")
    (pprint/pprint output-layer)
  )
)


(defn create-neural-network [single-tick-data]
  
  (let [
        input-layer (ilayer/create-input-layer single-tick-data)
        hidden-layer (hlayer/create-hidden-layer input-layer)        
        output-layer (olayer/create-output-layer hidden-layer)
       ]
    {:input-layer input-layer
     :hidden-layer hidden-layer
     :output-layer output-layer
    }
  )
)


;; referencing this page: http://galaxy.agh.edu.pl/~vlsi/AI/backp_t_en/backprop.html
(defn feed-forward
  "propagate price signal (start with bid) through the network"
  [neural-network]
  
  (let  [ nni (ilayer/calculate-value (:input-layer neural-network))
          nnh (hlayer/calculate-value nni (:hidden-layer neural-network))
          nno (olayer/calculate-value nnh (:output-layer neural-network))
        ]
    {:input-layer nni
     :hidden-layer nnh
     :output-layer nno
    }
  )
)
(defn calculate-total-error
  "in output neurons, calculate error between output (start with bid) and actual bid"
  [nlayer next-tick]
  
  (let [calculated-ask (-> nlayer first :calculated-value)
        actual-ask (-> next-tick second Double/parseDouble)
        ask-error (- calculated-ask actual-ask)
        ]
    ask-error
  )
)
(defn propogate-error
  "Propagate error back through the neural network"
  [neural-network total-error]
  
  (let [ nno (olayer/calculate-error (:hidden-layer neural-network) (:output-layer neural-network) total-error)
         nnh (hlayer/calculate-error (:input-layer neural-network) (:hidden-layer neural-network) nno)
         nni (ilayer/calculate-error (:input-layer neural-network) nnh)
       ]
    {
     :input-layer nni
     :hidden-layer nnh
     :output-layer nno
    }
  )
)

(defn weight-update-fn [loc update-constant]
                             (let [edge-weight (zip/node loc)
                                   pderiv (:partial-derivative edge-weight)]
                               (merge edge-weight
                                      { :weight (* -1 update-constant pderiv) } )
                             )
)
(defn update-weights
  "update the weights of the neural net"
  [neuralnet learning-constant]
  
  { :input-layer (layers/traverse-neural-layer learning-constant
                                               (:input-layer neuralnet)
                                               weight-update-fn)
    :hidden-layer (layers/traverse-neural-layer learning-constant
                                                (:hidden-layer neuralnet)
                                                weight-update-fn)
    :output-layer (layers/traverse-neural-layer learning-constant
                                                (:output-layer neuralnet)
                                                weight-update-fn)
  }
)




(use 'clojure.stacktrace)

(defn train [nnetwork tdata learning-constant target-error]
  
  (loop [neural-network nnetwork
         train-data tdata
         count 0]
  
    (let [next-tick (first train-data)        ;; next tick
          ff-nn (feed-forward neural-network) ;; feed forward
          terror (calculate-total-error (:output-layer ff-nn) next-tick) ;; total error
         ]
      
      (pprint/pprint (str "total-error[" terror "] / calculated-value[" (:calculated-value (first (:output-layer ff-nn))) "] / actual-value[" (-> next-tick second Double/parseDouble) "]"))
      
      ;; ** CHECK if finished
      (if (or (< (* -1 terror) target-error) (> count 10))
        ff-nn                             ;; return the trained neural-network
        (recur
         (update-weights                   ;; apply train algorithm & update weights
          (propogate-error ff-nn terror)
          learning-constant)
         (rest train-data)
         (+ count 1))
      )
    )
  )
)

(defn kickoff-training []

  (let [init-data (rest (config/load-train-data))
        next-tick (first init-data)
        nnetwork (create-neural-network next-tick)
        tdata (rest init-data)
       ]
    (train nnetwork
           tdata
           1.2
           0.1)
  )
)


(defn thing []
  
  (def nn2 (kickoff-training))
  (pprint/pprint nn2)
  
  
    ;; create neural network 
    (def train-data (config/load-train-data))
    (def first-tick (second train-data))
    (def neural-network (create-neural-network first-tick))
    
    ;; feed inputs forward
    (def nn (feed-forward neural-network))
    
    ;; get total error
    (def next-tick (nth train-data 2))
    (def terror (calculate-total-error (:output-layer nn) next-tick))
    
    ;; propagate error back through the neural network
    (def nn-back (propogate-error nn terror))
    
    
    (pprint/pprint (:output-layer nn))
    (pprint/pprint nn)
    (pprint/pprint nn-back)
    (pprint/pprint neural-network)
    
    
    ;; adjust weights for input, hidden and output values... 
    (def nn-wupdate (update-weights nn-back 1.2))
    
    (pprint/pprint (:output-layer nn-back))
    (pprint/pprint (:output-layer nn-wupdate))
    (pprint/pprint nn-wupdate)
    
    ;; train until an acceptable margin of error
    ;; ...
    
    ;;(def hist (conj iteration-history { :tick-data next-tick :neural-network nn }))
    
)

