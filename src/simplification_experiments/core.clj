(ns simplification-experiments.core
  (require [clojure.string :as str]
           [clojure.edn :as edn]
           [clojush.translate :as translate]
           [clojush.globals :as globals]
           [clojush.simplification :as simplification]
           [clojush.util]
           ))

; NOTE:
; normal GP run: 300 gens * 1000 inds = 300,000 evals
; simp exp: 100 trials * 10000 steps = 1,000,000 evals

(def trials 100)

(def steps-per-trial 10000)

(defn program-simplification-experiment
  "Uses program simplfiication multiple times to explore the outcomes."
  [logmap error-fn out-file]
  (spit out-file "Program simplification" :append true)
  (doseq [i (range trials)]
    (spit out-file
          (with-out-str
            (println "\n---------------")
            (println "Trial" i)
            (let [simp-ind (simplification/auto-simplify-from-program
                             (:program logmap)
                             error-fn
                             steps-per-trial
                             false ;whether to print or not during simplification
                             1)
                  simp-prog (:program simp-ind)
                  simp-test-errors (error-fn simp-prog :test)]
              (println "Simplified Program:" (pr-str simp-prog))
              (println "Simplified Program Size (points):" (clojush.util/count-points simp-prog))
              (println "Simplified Train Errors:" (:errors simp-ind))
              (println "Simplified Total Train Error:" (apply +' (:errors simp-ind)))
              (println "Simplified Test Errors:" simp-test-errors)
              (println "Simplified Total Test Error:" (apply +' simp-test-errors))))
          :append true)))

(defn genome-simplification-experiment
  "Uses genome simplfiication multiple times to explore the outcomes."
  [logmap error-fn out-file step-probabilities]
  (spit out-file (str "Genome simplification with step probabilities: "
                      (pr-str step-probabilities))
        :append true)
  (doseq [i (range trials)]
    (spit out-file
          (with-out-str
            (println "\n---------------")
            (println "Trial" i)
            (let [simp-ind (simplification/auto-simplify-plush
                             {:genome (:genome logmap)}
                             error-fn
                             steps-per-trial
                             0
                             step-probabilities)
                  simp-prog (:program simp-ind)
                  simp-test-errors (error-fn simp-prog :test)]
              (println "Simplified Genome:" (pr-str (:genome simp-ind)))
              (println "Simplified Genome Size (length):" (count (:genome simp-ind)))
              (println "Simplified Program:" (pr-str simp-prog))
              (println "Simplified Program Size (points):" (clojush.util/count-points simp-prog))
              (println "Simplified Train Errors:" (:errors simp-ind))
              (println "Simplified Total Train Error:" (apply +' (:errors simp-ind)))
              (println "Simplified Test Errors:" simp-test-errors)
              (println "Simplified Total Test Error:" (apply +' simp-test-errors))))
          :append true)))

(defn print-program-and-generalization
  "Prints information about program and generalization prior to simplification."
  [logmap error-fn]
  (println "Genome:" (pr-str (:genome logmap)))
  (println "Program:" (pr-str (:program logmap)))
  (println "Genome Size (length):" (count (:genome logmap)))
  (println "Program Size (points):" (clojush.util/count-points (:program logmap)))
  (let [train-errors (error-fn (:program logmap))
        test-errors (error-fn (:program logmap) :test)]
    (println "Train Errors:" train-errors)
    (println "Total Train Error:" (apply +' train-errors))
    (println "Test Errors:" test-errors)
    (println "Total Test Error:" (apply +' test-errors))
    (println)))

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
  (let [namespace (first args)
        simplification-type (read-string (second args))
        out-directory (str (nth args 2) "/")
        edn-files (drop 3 args)]
    (require (symbol (str "clojush.problems.software." namespace)))
    (reset! globals/global-max-points (:max-points (eval (symbol (str "clojush.problems.software." namespace "/argmap")))))
    (reset! globals/global-evalpush-limit (:evalpush-limit (eval (symbol (str "clojush.problems.software." namespace "/argmap")))))
    (doseq [edn-file edn-files]
      (let [logmap (add-translated-program-to-logmap (get-logmap-from-edn edn-file))
            error-fn (get-error-function namespace logmap)
            out-file (str out-directory
                          "simp-"
                          (first (take-last 2 (clojure.string/split edn-file #"/|\.")))
                          ".txt")]
        (spit out-file (with-out-str (print-program-and-generalization logmap error-fn)))
        (case simplification-type
          :program (program-simplification-experiment logmap error-fn out-file)
          :genome (genome-simplification-experiment logmap error-fn out-file
                                                    {{:silence 1} 0.5
                                                     {:silence 2} 0.3
                                                     {:silence 3} 0.1
                                                     {:silence 4} 0.1})
          :genome-backtracking (genome-simplification-experiment logmap error-fn out-file
                                                                 {{:silence 1} 0.4
                                                                  {:silence 2} 0.25
                                                                  {:silence 3} 0.1
                                                                  {:silence 4} 0.05
                                                                  {:silence 1 :unsilence 1} 0.05
                                                                  {:silence 2 :unsilence 1} 0.1
                                                                  {:silence 3 :unsilence 1} 0.05})
          :genome-noop (genome-simplification-experiment logmap error-fn out-file
                                                         {{:silence 1} 0.4
                                                          {:silence 2} 0.25
                                                          {:silence 3} 0.1
                                                          {:silence 4} 0.05
                                                          {:no-op 1} 0.1
                                                          {:no-op 2} 0.1})
          :genome-backtracking-noop (genome-simplification-experiment logmap error-fn out-file
                                                                      {{:silence 1} 0.3
                                                                       {:silence 2} 0.2
                                                                       {:silence 3} 0.1
                                                                       {:silence 1 :unsilence 1} 0.05
                                                                       {:silence 2 :unsilence 1} 0.1
                                                                       {:silence 3 :unsilence 1} 0.05
                                                                       {:no-op 1} 0.1
                                                                       {:no-op 2} 0.1})
          :else nil)))))
