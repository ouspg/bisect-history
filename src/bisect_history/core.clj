(ns bisect-history.core
  (require [me.raynes.conch :refer [programs]]
           [clojure.string :as string]
           [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(programs git)

(defn clean-ranges
  "Remove intermediate indices that have same value as neighbors."
  [idxs getter]
  (let [mids (filter
               #(not= % nil)
               (map #(when-not (apply = (map getter (list %1 %2 %3))) %2)
                    idxs (drop 1 idxs) (drop 2 idxs)))]
    (concat (list (first idxs)) mids (list (last idxs)))))

(defn subdivide
  "Subdivide the indices ranges if getter returns different values for the end points."
  [idxs getter]
  (let [cond-divide
        (fn [i j]
          (let [x (getter i)
                y (getter j)]
            (if (and (= x y) (> (- j i ) 1)) ; should the range be split?
              (list i)
              (list i (int (/ (+ i j) 2))))))]
    (flatten (conj (mapv cond-divide idxs (rest idxs)) (last idxs)))))

(defn memo-analysis-getter
  "Returns function that index of a commit in a list to the analysis result."
  [git-sh commits analyzer analyzer-args]
  (memoize
    (fn [id]
      (git-sh "checkout" (get commits id))
      (apply analyzer analyzer-args))))

(defn get-git-sh []
  (partial git "--no-pager"))

(defn exit [code message]
  (when message (println message))
  (System/exit code))

(def cli-options
  [["-n" "--number N" "number of commits to inspect" :parse-fn bigint :default 100]
   ["-d" "--divisions N" "max number of divisions" :parse-fn bigint :default 3]
   ["-s" "--short" "print short summary"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (string/join \newline
               ["Approximating the change of the specified attribute in the history of the Git repository"
                "Usage: bisect-history options ANALYZER_CLJ [ANALYZER_ARGS...]"
                ""
                "Options:"
                options-summary]))

(defn error-msg [errors]
  (str "Error in command line arguments:\n"
       (string/join \newline errors)))

(defn print-results
  [commits range-indices getter]
  (doseq [id (range (count commits))]
    (println (get commits id)
             (if (contains? (set range-indices) id)
               (getter id)
               ; Do the endpoints of the range the id lies have the same value?
               (if (= (getter (last  (take-while (partial > id) range-indices)))
                      (getter (first (drop-while (partial > id) range-indices))))
                 "|"
                 "?")))))

(defn print-summary
  [commits range-indices getter]
  (let [changes (filter #(apply not= (map getter %))
                        (map list range-indices (rest range-indices)))
        id-diff (map (partial apply #(list %1 (- (getter %1) (getter %2)))) changes)
        str-diff (map (partial apply #(str (get commits %1)
                                           " " (when (pos? %2) "+") %2)) id-diff)]
    (println (string/join \newline str-diff))))

(defn analyze-history
  [commits max-divisions git-sh analysis-getter]
    ; Iterate splitting the commit range to narrow down the changes in the inspected values
    (loop
      [iter-count 0
       ranges [0 (dec (count commits))]]
      (if-not (< iter-count max-divisions)
        ranges
        (recur (inc iter-count) (clean-ranges (subdivide ranges analysis-getter)
                                              analysis-getter)))))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        max-divisions (:divisions options)
        [analyzer-file & analyzer-args]  arguments]
    (cond
      (or (:help options) (nil? analyzer-file)) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    (let [git-sh (get-git-sh)
          analyzer (load-file analyzer-file)
          rev-list-args (filter identity
                                ["rev-list"
                                 "--first-parent"
                                 (when (:number options) (str "--max-count=" (:number options)))
                                 "master"])
          master-commits (string/split-lines (apply git-sh rev-list-args))
          analysis-getter (memo-analysis-getter git-sh master-commits analyzer analyzer-args)
          analyzed-ranges
          (vec (analyze-history master-commits max-divisions git-sh analysis-getter))]
   (if (:short options)
     (print-summary master-commits analyzed-ranges analysis-getter)
     (print-results master-commits analyzed-ranges analysis-getter))))
   (exit 0 nil))
