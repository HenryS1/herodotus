(in-package :herodotus)

(defun make-parser-name (class-name-symbol)
  (let ((name (symbol-name class-name-symbol)))
    (intern (concatenate 'string name "-PARSER"))))

(defun make-keyword (sym)
  (let ((name (symbol-name sym)))
    (intern name "KEYWORD")))

(defun camel-case (str)
  (let ((parts (cl-ppcre:split "-+" str)))
    (format nil "~{~a~}" (cons (car parts) 
                               (mapcar #'string-capitalize (cdr parts))))))

(defun snake-case (str)
  (cl-ppcre:regex-replace-all "-" str "_"))

(defun screaming-snake-case (str)
  (string-upcase (lisp-case-to-snake-case str)))

(defun make-keys (slots case-fn)
  (mapcar (lambda (s) (funcall case-fn (string-downcase (symbol-name s)))) slots))

(defmacro define-parser (class-name slots case-fn)
  (let* ((keys (make-keys slots case-fn))
         (init-args (mapcar #'make-keyword slots))
         (parser-name (make-parser-name class-name)))
    (alexandria:with-gensyms (json-obj)
      (let ((constructor-params 
             (apply #'append (loop for key in keys
                                for init-arg in init-args 
                                collect (list init-arg 
                                              (list 'gethash key json-obj))))))
        `(defun ,parser-name (json)
           (let ((,json-obj (yason:parse json)))
             (make-instance ',class-name ,@constructor-params)))))))

(defmacro define-encoder (class-name slots case-fn)
  (let ((keys (make-keys slots case-fn)))
    (alexandria:with-gensyms (clos-obj)
      (let ((encoder-parameters 
             (loop for slot in slots
                for key in keys
                collect (list 'herodotus:encode-object-element key 
                              (list slot clos-obj)))))
        `(defmethod herodutus:encode-slots progn ((,clos-obj ,class-name))
                    ,@encoder-parameters)))))

(defmacro define-serialiser (class-name slots case-fn)
  `(progn 
     (define-encoder ,class-name ,slots ,case-fn)
     (define-json-parser ,class-name ,slots ,case-fn)))
