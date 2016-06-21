(ns scan-build-parser
  (require [clojure.java.io :as io]
           [clojure.string :refer [trim]]
           [clj-json [core :as json]]
           [me.raynes.conch :refer [programs]]))

(programs bash rm git)

(defn commit-file [dir commit]
  (io/file dir (str commit ".json")))

(defn parser
  [scan-result]
  ; Parse errors straight from the scan-build output
  ; Warning format is file:row:col: warning: description message
  (let [warning-pattern #"(.+):([0-9]+):([0-9]+): (.+): (.*)"
        result-dir-pattern #"scan-build: Analysis results \(plist files\) deposited in '(.*)'"
        scan-build-report-dirs (map second (re-seq result-dir-pattern (:stdout scan-result)))
        warnings (map rest (re-seq warning-pattern (:stderr scan-result)))]
    ; Remove report dirs as they are not needed
    (if scan-build-report-dirs
      (apply rm "-rf" scan-build-report-dirs)
      (println "scan-build report dir was not removed" (:stdout scan-result)))
    warnings))

(defn load-commit-analysis
  "Return loaded report from a file or a nil."
  [dir commit]
  ; If report file already exists serve it instead
  (let [f (commit-file dir commit)]
    (when (.exists f)
      (json/parse-string (slurp f)))))

(defn analyze-commit
  [build-script dir commit]
  (println "analyzing " commit)
  (let [scan-result (bash build-script {:verbose true})
        warnings (parser scan-result)]
    ; Store analysis result
    (spit (commit-file dir commit) (json/generate-string warnings)))
  ; After analysis finishes load the freshly generated analysis
  (load-commit-analysis dir commit))

(defn analyzer
  "Return warning count."
  [build-script dir]
  (let [commit (trim (git "rev-list" "-n1" "HEAD"))] ; get the hash of the current commit
  (count
    (or (load-commit-analysis dir commit)            ; hit in previous results?
        (analyze-commit build-script dir commit))))) ; run the analysis
