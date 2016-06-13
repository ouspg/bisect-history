(ns scan-build-parser
  (require [me.raynes.conch :refer [programs]]))

(programs rm)

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
