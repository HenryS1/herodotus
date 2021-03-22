(defpackage :herodotus/tests
  (:use :cl :herodotus :rove))

(in-package :herodotus/tests)

(define-json-model test-point (x y))

(deftest define-json-model
  (testing "should create a class with accessors and initargs"
    (let ((point (make-instance 'test-point :x 1 :y 2)))
      (ok (= (x point) 1))
      (ok (= (y point) 2))))

  (testing "should create a package containing a from-json function"
    (let ((point (test-point-json:from-json "{ \"x\": 1, \"y\": 2 }")))
      (ok (= (x point) 1))
      (ok (= (y point) 2))))
  
  (testing "should implement the generic method to-json for a class"
    (let ((point (make-instance 'test-point :x 1 :y 2)))
      (ok (string= (herodotus:to-json point) "{\"x\":1,\"y\":2}")))))

(define-json-model test-grid ((points () test-point)))

(deftest from-json
  (testing "should parse a vector of nested classes in an object"
    (let ((grid (test-grid-json:from-json "{ \"points\": [{\"x\": 0, \"y\": 5}, {\"x\": 1, \"y\": 3}] }")))
      (ok (vectorp (points grid)))
      (ok (= (length (points grid)) 2))
      (let ((p1 (aref (points grid) 0))
            (p2 (aref (points grid) 1)))
        (ok (= (x p1) 0))
        (ok (= (y p1) 5))
        (ok (= (x p2) 1))
        (ok (= (y p2) 3))))))


