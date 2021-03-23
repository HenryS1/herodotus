(defpackage herodotus
  (:use :cl :yason)
  (:export
   #:to-json
   #:define-json-model))

(in-package :herodotus)

(defgeneric to-json (class))

(defun json-package-name (class-name)
  (concatenate 'string (symbol-name class-name) "-JSON"))

(defun make-keyword (sym)
  (let ((name (symbol-name sym)))
    (intern name "KEYWORD")))

(defun camel-case (str)
  (let ((parts (cl-ppcre:split "-+" str)))
    (format nil "~{~a~}" (cons (car parts) 
                               (mapcar #'string-capitalize (cdr parts))))))

(defun snake-case (str)
  (cl-ppcre:regex-replace-all "-" str "_"))

(defun make-hash-parser-name (class-name)
  (let ((name (json-package-name class-name)))
    (if (find-package name)
        (intern "FROM-HASH" name)
        (error (format nil "Could not find parser for class ~a. Please define a json model for it." class-name)))))

(defun screaming-snake-case (str)
  (string-upcase (snake-case str)))

(defun has-custom-key (x) 
  (and (consp x) (cadr x)))

(defun make-keys (slots case-fn)
  (mapcar (lambda (slot) 
            (if (has-custom-key slot)
                (cadr slot)
                (funcall case-fn (string-downcase (symbol-name (get-slot slot)))))) 
          slots))

(defun has-object-constructor (slot) 
  (and (consp slot) (caddr slot)))

(defun make-standard-slot (init-arg key json-obj)
  `(,init-arg (gethash ,key ,json-obj)))

(defun get-slot-type (slot) (caddr slot))

(defun make-object-slot (init-arg key slot json-obj)
  (let* ((hash-parser-name (make-hash-parser-name (get-slot-type slot))))
    `(,init-arg (,hash-parser-name (gethash ,key ,json-obj)))))

(defmacro define-json-constructor (class-name slots case-fn)
  (let* ((keys (make-keys slots case-fn))
         (init-args (mapcar #'make-init-arg slots))
         (hash-parser-name (make-hash-parser-name class-name)))
    (alexandria:with-gensyms (json-obj)
      (let ((constructor-params 
             (apply #'append 
                    (loop for key in keys
                       for init-arg in init-args 
                       for slot in slots
                       collect (if (has-object-constructor slot)
                                   (make-object-slot init-arg key slot json-obj)
                                   (make-standard-slot init-arg key json-obj))))))
        `(defun ,hash-parser-name (,json-obj)
           (handler-case 
               (cond 
                 ((null ,json-obj) (vector))
                 ((vectorp ,json-obj)
                  (map 'vector #',hash-parser-name ,json-obj))
                 (t (make-instance ',class-name ,@constructor-params)))))))))

(defmacro define-parser (class-name slots case-fn)
  (let* ((hash-parser-name (make-hash-parser-name class-name)))
    (alexandria:with-gensyms (json-obj)
      `(progn 
           (define-json-constructor ,class-name ,slots ,case-fn)
           (defun ,(intern "FROM-JSON" (json-package-name class-name)) (json)
             (let* ((yason:*parse-json-arrays-as-vectors* t) 
                    (,json-obj (yason:parse json)))
               (,hash-parser-name ,json-obj)))))))

(defun slot-accessor (slot-description)
  (if (consp slot-description)
      (car slot-description)
      slot-description))

(defmacro define-encoder (class-name slots case-fn)
  (format t "class-name ~a slots ~a~%" class-name slots)
  (let ((keys (make-keys slots case-fn)))
    (alexandria:with-gensyms (clos-obj)
      (let ((encoder-parameters 
             (loop for slot in slots
                for key in keys
                collect (list 'yason:encode-object-element key 
                              (list (slot-accessor slot) clos-obj)))))
        `(progn (defmethod yason:encode-slots progn ((,clos-obj ,class-name))
                           ,@encoder-parameters)
                (defmethod herodotus:to-json ((,clos-obj ,class-name))
                   (yason:with-output-to-string* ()
                     (encode-object ,clos-obj))))))))

(defun get-slot (slot-spec) 
  (if (consp slot-spec)
      (car slot-spec)
      slot-spec))

(defun make-init-arg (slot-spec) 
  (make-keyword (get-slot slot-spec)))

(defun get-slot-defs (slot-specs)
  (loop for slot-spec in slot-specs
     collect (let ((slot (get-slot slot-spec))
                   (init-arg (make-init-arg slot-spec)))
               `(,slot :accessor ,slot :initarg ,init-arg))))

(defun select-case-function (case-type)
  (case case-type
    (:camel-case #'camel-case)
    (:snake-case #'snake-case)
    (:screaming-snake-case #'screaming-snake-case)
    (:kebab-case #'identity)
    (t (error (format nil "Unknown case-type ~a, expected one of :camel-case, :snake-case, :screaming-snake-case or :kebab-case" case-type)))))

(defmacro within-package (package-name &rest body)
  (let ((initial-package (package-name *package*)))
    `(progn 
       (in-package ,package-name)
       ,@body
       (in-package ,initial-package))))

(defmacro define-json-model (name slots &optional (case-type :camel-case))
  (let ((slot-defs (get-slot-defs slots))
        (case-fn (select-case-function case-type))
        (initial-package (package-name *package*)))
    `(progn 
       (defclass ,name () ,slot-defs)
       (defpackage ,(json-package-name name) 
         (:use :cl :herodotus :yason)
         (:export #:from-json #:to-json #:from-hash))
       (in-package ,(json-package-name name))
       (define-encoder ,name ,slots ,case-fn)
       (define-parser ,name ,slots ,case-fn)
       (in-package ,initial-package))))
