(ns simplification-experiments.core
  (require [clojure.string :as str]
           [clojure.edn :as edn]
           [clojush.translate :as translate]
           ;[clojush.problems.software.replace-space-with-newline :refer [replace-space-input]]
           ))



(defn get-error-function
  [namespace logmap]
  (let [error-fn-maker (eval (symbol (str "clojush.problems.software." namespace
                                          "/make-" namespace "-error-function-from-cases")))]
    (error-fn-maker (:training-cases logmap)
                    (:test-cases logmap))))

(defn add-translated-program-to-logmap
  "Translates the genome of the logmap into a program and adds it to the logmap"
  [logmap]
  (assoc logmap
         :program
         (translate/translate-plush-genome-to-push-program logmap
                                                           {:max-points 100000})))

(defn get-logmap-from-edn
  "Reads in EDN file and returns the map of the important data from the log."
  [file]
  (edn/read-string (slurp file)))

(defn -main
  "Arguments are EDN files to process."
  [& args]
  ;(println (replace-space-input 10))
  (let [namespace (first args)
        edn-files (rest args)]
    (require (symbol (str "clojush.problems.software." namespace)))
    (doseq [edn-file edn-files]
      (let [logmap (add-translated-program-to-logmap (get-logmap-from-edn edn-file))
            error-fn (get-error-function namespace logmap)]
        (println (error-fn '(in1 print_string)))
        ;(println (error-fn (:program logmap))) ;should be perfect -- MAYBE NEED TO UPDATE EVALPUSH AND MAX-POINTS???
        (System/exit 0)))))


