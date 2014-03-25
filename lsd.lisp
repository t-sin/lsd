#|
  This file is a part of lsd project.
|#

(in-package :cl-user)
(defpackage lsd
  (:use :cl
        :cl-annot)
  (:import-from :babel
                :octets-to-string)
  (:import-from :unix-options
                :getopt
                :cli-options)
  (:import-from :binary-file
                :read-bytes
                :read-integer))
(in-package :lsd)

(cl-annot:enable-annot-syntax)



(defconstant +app-name+ "lsd - lsd sound decoder")
(defconstant +usage+ "usage: lsd [lsdfile]")

(defconstant +lsd-max-file-number+ 48)

(defconstant +magic+ 4)
(defconstant +lsd-header-name+ 32)
(defconstant +lsd-header-size+ 4)
(defconstant +lsd-wave-head+ 16)
(defconstant +lsd-wave-offset+ 2)



(defun read-bytes-as-string (in num)
  (remove #\Null (octets-to-string (read-bytes in num))))

(defun parse-lsd (in)
  (list (cons :file-size (file-length in))
        (cons
         :file-info
         (progn (file-position in +magic+)
                (loop
                   repeat +lsd-max-file-number+
                   if (> (file-length in) (file-position in))
                   collect (let ((name (read-bytes-as-string in +lsd-header-name+))
                                (size (read-integer in :bytes +lsd-header-size+ :signed nil))
                                (header-start (file-position in)))
                            (file-position in (+ (file-position in) +lsd-wave-head+ +lsd-wave-offset+ size))
                            (vector name size
                                    (cons header-start
                                          (+ header-start +lsd-wave-head+)) ; header-end
                                    (cons (+ header-start +lsd-wave-head+)
                                          (+ header-start +lsd-wave-head+ +lsd-wave-offset+ size)))))))))


;; lsd->wave*
(defun write-waves (lsdpath)
  (with-open-file (in lsdpath :direction :input :element-type '(unsigned-byte 8))
    (if (string= "LSD" (read-bytes-as-string in +magic+))
        (let ((files (cdr (nth 1 (parse-lsd in)))))
          (flet ((get-data (file-info n)
                   (let* ((pos (car (svref file-info n)))
                          (size (- (cdr (svref file-info n)) pos)))
                     (file-position in pos)
                     (read-bytes in size))))
            (loop for f in files
                 do (write-wave (svref f 0)
                                (get-data f 2)
                                (get-data f 3)))))
        ;; (vector name head-bytes data-bytes)
        (format t "~s is not lsd file.~%" lsdpath))))

(defun write-wave (name header data)
  nil)

(defun check-path (path)
  (and (string= "lsd" (string-downcase (pathname-type path)))
       (probe-file path)))

@export
(defun lsd (path)
  (if (check-path path)
      (write-waves path)
      (format t "~S is not lsd file.~%~%" path)))

@export
(defun app ()
  (multiple-value-bind (_ _ args)
      (getopt (cli-options) nil nil)
    (format t "~%~A~%~%" +app-name+)
    (lsd (car args))))
