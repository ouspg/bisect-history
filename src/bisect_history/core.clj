(ns bisect-history.core
  (require [me.raynes.conch :refer [programs]]
           [clojure.java.io :as io]
           [clojure.string :as string]
           [clojure.tools.cli :refer [parse-opts]]
           [clj-json [core :as json]])
  (:gen-class))

(programs git bash rm)

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

(defn commit-file [dir commit]
  (io/file dir (str commit ".json")))

(defn load-commit-analysis
  "Return report loaded from a file or a nil."
  [dir commit]
  ; If report file already exists serve it instead
  (let [f (commit-file dir commit)]
    (when (.exists f)
      (json/parse-string (slurp f)))))

(defn analyze-commit
  [git-sh build-script parser dir commit]
  (println "analyzing " commit)
  (git-sh "checkout" commit)
  (let [scan-result (bash build-script {:verbose true})
        warnings (parser scan-result)]
    ; Store analysis result
    (spit (commit-file dir commit) (json/generate-string warnings)))
  ; After analysis finishes load the freshly generated analysis
  (load-commit-analysis dir commit))

(defn get-commit-analysis
  [git-sh build-script parser dir commit]
  (or (load-commit-analysis dir commit) ; hit in previous results?
      (analyze-commit git-sh build-script parser dir commit)))   ; run the analysis

(defn get-memo-warning-count-getter
  [git-sh build-script parser dir commits]
  (memoize
    (fn
      [id]
      (count (get-commit-analysis git-sh build-script parser dir (get commits id))))))

(defn get-git-sh []
  (partial git "--no-pager"))

(defn exit [code message]
  (when message (println message))
  (System/exit code))

(def cli-options
  [["-n" "--number N" "number of commits to inspect" :parse-fn bigint :default 100]
   ["-d" "--divisions N" "max number of divisions" :parse-fn bigint :default 3]
   ["-o" "--out-dir PATH" "directory for storing reports"
    :validate [#(.isDirectory (io/file %)) "is not directory"]]
   ["-s" "--short" "print short summary"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (string/join \newline
               ["A tool for approximating change of attribute in the history of a Git repository"
                "Usage: bisect-history options BUILD-SCRIPT PARSER"
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

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        git-sh (get-git-sh)
        analysis-dir  (:out-dir options)
        max-divisions (:divisions options)
        [build-script parser-file & _]  arguments]
    (cond
      (or (:help options) (nil? build-script) (nil? parser-file)) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    (let [warning-parser (load-file parser-file)
          rev-list-args (filter identity
                                ["rev-list"
                                 "--first-parent"
                                 (when (:number options) (str "--max-count=" (:number options)))
                                 "master"])
          master-commits (string/split-lines (apply git-sh rev-list-args))
          warn-count-getter (get-memo-warning-count-getter git-sh
                                                           build-script
                                                           warning-parser
                                                           analysis-dir
                                                           master-commits)]
      ; Iterate splitting the commit range to narrow down the changes in the inspected values
      (loop
        [iter-count 0
         ranges [0 (dec (count master-commits))]]
        (if (< iter-count max-divisions)
          (let [new-ranges (clean-ranges (subdivide ranges warn-count-getter)
                                         warn-count-getter)]
            (recur (inc iter-count) new-ranges))
          (let [analyzed-ranges (vec ranges)]
            (if (:short options)
              (print-summary master-commits analyzed-ranges warn-count-getter)
              (print-results master-commits analyzed-ranges warn-count-getter)))))))
  (exit 0 nil))
