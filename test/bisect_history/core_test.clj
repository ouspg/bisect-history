(ns bisect-history.core-test
  (:require [clojure.test :refer :all]
            [bisect-history.core :refer :all]
            [clojure.string :as string])
  (:import [java.io BufferedReader StringReader]))
(deftest subdivide-test
  (testing "subdivide with a list"
    (let [values '(1 1 1 1 1 3 3 4 5)
          getter (partial nth values)
          iter0 (list 0 (dec (count values)))
          iter1 (subdivide iter0 getter)
          iter2 (subdivide iter1 getter)
          iter3 (subdivide iter2 getter)]

      (is (= iter3 '(0 4 5 6 7 8))))))

(deftest clean-ranges-test
  (testing "test cleaning ranges"
    (let [values '(1 1 1 1 1 3 3 3 4 5)
          ranges (range (count values))
          cleaned (clean-ranges ranges (partial nth values))]
      (is (= cleaned '(0 4 5 7 8 9))))))


(deftest parse-scan-build-output-test
  (testing "test parsing scan-build output"
    (let [output (->> ["path/to/file.c:100:20: warning: Value stored to 'var' is never read."
                       "  var = foo"
                       "  ~   ^"
                       "malformed:index:index: warning: Should not parse"
                       "another/file/path.c:233:54: warning: foobar"]
                      (#(string/join \newline %))) 
          parsed (parse-scan-build-output output)]
      (is (= parsed [["path/to/file.c" "100" "20" "warning" "Value stored to 'var' is never read."]
                     ["another/file/path.c" "233" "54" "warning" "foobar"]])))))
