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

(define-json-model test-stock (prices))

(define-json-model test-dictionary (entries))

(define-json-model test-player (name (location test-point)))

(define-json-model test-grid ((points test-point)))

(deftest from-json 
  (testing "should parse vectors of builtin types"
    (let ((stock (test-stock-json:from-json "{ \"prices\": [1, 2, 3] }")))
      (ok (vectorp (prices stock)))
      (ok (equalp (prices stock) #(1 2 3)))))

  (testing "should parse hash tables of builtin types"
    (let ((dictionary (test-dictionary-json:from-json "{ \"entries\": { \"bird\": \"flying animal\", \"river\": \"moving inland body of water\" } }")))
      (ok (equalp (entries dictionary) 
                  (alexandria:alist-hash-table '(("bird" . "flying animal") ("river" . "moving inland body of water")) :test 'equal)))))

  (testing "should parse a nested class in an object"
    (let ((player (test-player-json:from-json "{ \"name\": \"bob\", \"location\": { \"x\": 11, \"y\": 5} }")))
      (ok (equal (name player) "bob"))
      (let ((location (location player)))
        (ok (= (x location) 11))
        (ok (= (y location) 5)))))
  
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

(define-json-model test-snake (snake-case) :snake-case)

(deftest snake-case
  (testing "should read an object formatted in snake case"
    (let ((snake (test-snake-json:from-json "{ \"snake_case\": \"boa\" }")))
      (ok (equal (snake-case snake) "boa")))))

(define-json-model test-camel (camel-case) :camel-case)

(deftest camel-case
  (testing "should read an object formatted in camel case"
    (let ((camel (test-camel-json:from-json "{ \"camelCase\": \"ship of the desert\"}")))
      (ok (equal (camel-case camel) "ship of the desert")))))

(define-json-model test-screaming-snake (screaming-snake-case) :screaming-snake-case)

(deftest screaming-snake-case
  (testing "should read an object formatted in screaming snake case"
    (let ((screaming-snake (test-screaming-snake-json:from-json "{ \"SCREAMING_SNAKE_CASE\": \"screech\" }")))
      (ok (equal (screaming-snake-case screaming-snake) "screech")))))

(define-json-model test-kebab (kebab-case) :kebab-case)

(deftest kebab-case
  (testing "should read an object formatted in kebab case"
    (let ((kebab (test-kebab-json:from-json "{ \"kebab-case\": \"skewered zucchini\" }")))
      (ok (equal (kebab-case kebab) "skewered zucchini"))))) 

(define-json-model test-names ((custom-name () "Custom_Name") ordinary-name) :camel-case)

(deftest custom-name
  (testing "should read custom names in the format they are provided"
    (let ((names (test-names-json:from-json "{ \"Custom_Name\": \"Dweezil\", \"ordinaryName\": \"Bob\" }")))
      (ok (equal (custom-name names) "Dweezil"))
      (ok (equal (ordinary-name names) "Bob"))))

  (testing "should write fields with the format provided in a custom name"
    (let ((names (make-instance 'test-names :custom-name "Dweezle" :ordinary-name "Bob")))
      (ok (equal (herodotus:to-json names) "{\"Custom_Name\":\"Dweezle\",\"ordinaryName\":\"Bob\"}")))))

(deftest encode-nested-class
  (testing "should encode a class inside a class"
    (let ((test-player (make-instance 'test-player
                                      :name "player" 
                                      :location (make-instance 'test-point :x 1 :y 10))))
      (ok (equalp (herodotus:to-json test-player) 
                  "{\"name\":\"player\",\"location\":{\"x\":1,\"y\":10}}")))))

(deftest nested-class-without-parser
  (testing "should raise an error during class definition"
    (handler-case 
       (progn (eval '(define-json-model test-no-parser ((things not-parseable))))
              (fail "An error should be raised for a nested class without a parser"))
      (error (err)
        (if (search "Could not find parser" (format nil "~a" err))
            (pass "Got the expected error for an undefined parser")
            (fail (format nil "Unexpected error. Expected parser missing error, got: ~a" err)))))))

(deftest serialize-list
    (testing "should serialize list to a json array"
      (ok (equal (herodotus:to-json '(1 2 3)) "[1,2,3]"))))

(deftest serialize-vector
    (testing "should serialize a vector to a json array"
      (ok (equal (herodotus:to-json #(1 2 3)) "[1,2,3]"))))

(deftest serialize-hash-table
    (let ((table (make-hash-table :test 'equal)))
      (setf (gethash "name" table) "john")
      (setf (gethash "age" table) 60)
      (testing "should serialize a hash-table to a json object"
        (ok (equal (herodotus:to-json table) "{\"name\":\"john\",\"age\":60}")))))

(deftest serialize-number 
  (ok (equal (herodotus:to-json 1) "1")))

(deftest serialize-true
  (ok (equal (herodotus:to-json t) "true")))

(deftest serialize-nil
  (ok (equal (herodotus:to-json nil) "null")))

(deftest serialize-string
  (ok (equal (herodotus:to-json "string") "\"string\"")))
