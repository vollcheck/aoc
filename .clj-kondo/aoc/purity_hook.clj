(ns purity-hook
  (:require [clj-kondo.hooks-api :as api]))

(defn defpure
  "Macro analysis for `purity/defpure."
  [{:keys [node]}]
  #_{:clj-kondo/ignore [:unused-binding]}
  (let [[fn-sym fn-name tests & body] (:children node)
        fn-sym (if (= (str fn-sym) "defpure")
                 (api/token-node 'defn)
                 fn-sym)

        ;; [old-props body] (if (api/map-node? (first body))
        ;;                    [(-> body first :children) (next body)]
        ;;                    [nil body])
        expanded (api/list-node
                  (list* fn-sym fn-name body))]
    {:node (with-meta expanded (meta node))}))

(comment
  (def defpure-string
    "(defpure square
    {[0] 0
     [2] 4
     [3] 9}
    \"squares its input\"
    [^Number x]
    (* x x))")

  (def r (api/parse-string defpure-string))

  (str (:node (defpure {:node (api/parse-string defpure-string)})))
  )
