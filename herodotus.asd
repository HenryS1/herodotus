(defsystem herodotus
  :name "Herodotus"
  :author "Henry Steere"
  :version "0.0.1"
  :maintainer "henry.steere@gmail.com"
  :license "BSD"
  :description "Wrapper around Yason JSON parser/encoder with convenience methods for CLOS"
  :long-description "Provides a define-serialiser macro that defines both an encoder and decoder for a common lisp class. Allows one to easily specify case convention for fields in a JSON object as either snake case, camel case, or screaming snake case (with apologies to the rust library serde)."
  :depends-on (:yason :alexandria :cl-ppcre)
  :components ((:file "herodotus")))
