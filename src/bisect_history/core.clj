(ns bisect-history.core
  (require [me.raynes.conch :refer [programs]]
           [clojure.java.io :as io]
           [clojure.string :as string]
           [clojure.tools.cli :refer [parse-opts]]
           [clj-json [core :as json]])
  (:gen-class))

(programs git scan-build rm)

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

(defn parse-scan-build-output
  [scan-log]
  ; Parse errors straight from the scan-build output
  ; Warning format is file:row:col: warning: description message
  (let [warning-pattern #"(.+):([0-9]+):([0-9]+): (.+): (.*)"]
    (map rest (re-seq warning-pattern scan-log))))

(defn analyze-commit
  [git-sh scan-build-sh config-cmd dir commit]
  ; Run scan-build recipe
  (println "analyzing " commit)
  (git-sh "checkout" commit)
  (scan-build-sh "make clean") ; make sure it is clean to rebuild
  (apply scan-build-sh config-cmd)

  (let [scan-result (scan-build-sh "make" "-s" "-j2" {:verbose true})
        warnings (parse-scan-build-output (:stderr scan-result))
        result-dir #"scan-build: Analysis results \(plist files\) deposited in '(.*)'"
        [_ scan-build-report-dir] (re-find result-dir (:stdout scan-result))]
    (spit (commit-file dir commit) (json/generate-string warnings))
    ; Remove analysis files (is there a way to prevent scan-build from storing them?)
    (if scan-build-report-dir
      (rm "-rf" scan-build-report-dir)
      (println "scan-build report dir was not removed" (:stdout scan-result))))

  (scan-build-sh "make clean") ; clean after

  ; After analysis finishes load the freshly generated analysis
  (load-commit-analysis dir commit))

(defn get-commit-analysis
  [git-sh scan-build-sh config-cmd dir commit]
  (or (load-commit-analysis dir commit) ; hit in previous results?
      (analyze-commit git-sh scan-build-sh config-cmd dir commit)))   ; run the analysis

(defn get-memo-warning-count-getter
  [git-sh scan-build-sh config-cmd dir commits]
  (memoize
    (fn
      [id]
      (count (get-commit-analysis git-sh scan-build-sh config-cmd dir (get commits id))))))

(defn get-git-sh []
  (partial git "--no-pager"))

(defn get-scan-build-sh [out-dir]
  (partial scan-build "-o" out-dir "-k" "-plist"))

(defn exit [code message]
  (when message (println message))
  (System/exit code))

(def cli-options
  [;["-C" "--git-path PATH" "Git repository path"
   ;:default "."
   ;:validate [#(.isDirectory (io/file %)) "PATH is not a directory"]]
   ["-n" "--number N" "max number of commits to inspect" :parse-fn bigint :default 1000]
   ["-d" "--divisions N" "max number of divisions" :parse-fn bigint :default 3]
   ["-o" "--out-dir PATH" "directory for storing reports"
    :validate [#(.isDirectory (io/file %)) "is not directory"]]
   ["-r" "--scan-build-report-dir PATH" "directory for scan-build reports"
    :validate [#(.isDirectory (io/file %)) "is not directory"]]
   ["-c" "--config-cmd CMD" "configuration command to execute before make"
    :parse-fn #(string/split % #"\s+") :default ["./config"]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (string/join \newline
               ["A tool for approximating change of attribute in the history of a Git repository"
                "Usage: bisect-history options"
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
               (do
                 (if (= (getter (last  (take-while (partial > id) range-indices)))
                        (getter (first (drop-while (partial > id) range-indices))))
                   "|"
                   "?"))))))


(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        git-sh (get-git-sh)
        scan-build-sh (get-scan-build-sh (:scan-build-report-dir options))
        analysis-dir  (:out-dir options)
        max-divisions (:divisions options)]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    (let [rev-list-args (filter identity
                                ["rev-list"
                                 "--first-parent"
                                 (when (:number options) (str "--max-count=" (:number options)))
                                 "master"])
          master-commits (string/split-lines (apply git-sh rev-list-args))
          warn-count-getter (get-memo-warning-count-getter git-sh
                                                           scan-build-sh
                                                           (:config-cmd options)
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
            (print-results master-commits analyzed-ranges warn-count-getter))))))
  (exit 0 nil))
