(ns tailrecursion.warp-test
  (:require
   [clojure.test :refer :all]
   [tailrecursion.warp :refer :all]))

(deftest test-try*
  (testing "try* returns correct value when no exceptions thrown"
    (is (= 100 (try* (/ 200 2)))))
  (testing "try* doesn't throw when there's an exception if throw* isn't called"
    (is (= nil (try* (/ 1 0))))
    (is (= 100 (try* (/ 1 0) 100))))
  (testing "try* correctly processes finally clause"
    (is (= "x" (with-out-str (try* (/ 1 0) 100 (print "x")))))))

(deftest test-throw*
  (testing "throw* causes try* to throw"
    (is (= 100 (try (try* (/ 1 0) (throw*)) (catch Throwable _ 100))))))

(deftest test-or*
  (testing "or* can be called with no arguments"
    (is (= nil (or*))))
  (testing "or* returns the value of the first non-throwing clause"
    (is (= 100 (or* (/ 1 0) 100 (/ 2 0) 200)))
    (is (= 100 (or* (/ 1 0) (/ 2 0) 100 200)))))
