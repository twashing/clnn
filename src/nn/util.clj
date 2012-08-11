(ns nn.util)

(defn generate-id
  "generate system wide unique ID"
  []
  (.. (java.rmi.dgc.VMID.) toString (replaceAll ":" "") (replaceAll "-" ""))
)
