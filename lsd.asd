#|
  This file is a part of lsd project.
|#

(in-package :cl-user)
(defpackage lsd-asd
  (:use :cl :asdf))
(in-package :lsd-asd)

(defsystem lsd
  :version "0.1"
  :author "t-sin"
  :license "NYSL"
  :depends-on (:cl-annot)
  :components (:file "lsd")
  :description "extracting sound from lsd file, for a certain Dojin circle SDK"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op lsd-test))))
