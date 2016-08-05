(ns simplification-experiments.core
  (require [clojure.string :as str]
           [clojure.edn :as edn]
           [clojush.translate :as translate]
           ;[clojush.problems.software.replace-space-with-newline :refer [replace-space-input]]
           ))





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
  (doseq [edn-file args]
    (prn (take 5 (:training-cases (add-translated-program-to-logmap (get-logmap-from-edn edn-file)))))
    (System/exit 0)))
