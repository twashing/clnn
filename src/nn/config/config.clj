(ns nn.config.config

  (:require [clojure-csv.core :as csv]
            [clojure.java.io :as io]
            )
)

(defn load-train-data []
  
  (let [config (load-file "etc/config.clj")
        dname (-> config :data :test)
       ]
  
    (csv/parse-csv (io/reader dname))
  )
)


