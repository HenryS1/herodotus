(defpackage :herodotus/tests
  (:use :cl :herodotus :rove))

(in-package :herodotus/tests)

(define-json-model test-point (x y))

(deftest define-data-class
  (testing "should create a class with accessors and initargs"
    (let ((point (make-instance 'test-point :x 1 :y 2)))
      (ok (= (x point) 1))
      (ok (= (y point) 2)))))

(deftest define-data-class
  (testing "should create a package containing a from-json function"
    (let ((point (test-point-json:from-json "{ \"x\": 1, \"y\": 2 }")))
      (ok (= (x point) 1))
      (ok (= (y point) 2)))))

