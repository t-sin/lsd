#|
  This file is a part of lsd project.
|#

(in-package :cl-user)
(defpackage lsd
  (:use :cl
        :cl-annot)
  (:import-from :babel
                :octets-to-string
                :string-to-octets)
  (:import-from :unix-options
                :getopt
                :cli-options)
  (:import-from :binary-file
                :read-bytes
                :read-integer
                :write-integer))
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

(defconstant +wave-file-extention+ ".wav")


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
                                    ;; (start-pos . length)
                                    (cons header-start
                                          +lsd-wave-head+)
                                    (cons (+ header-start +lsd-wave-head+)
                                          (+ size +lsd-wave-offset+)))))))))

(defun print-wave-header (header)
  (ccl:with-input-from-vector (in header)
    (flet ((print-info (name bytes)
             (format t "  ~A: ~S~%"
                     name (read-integer in :bytes bytes))))
      (print-info "format-id" 2)
      (print-info "cannels" 2)
      (print-info "sampling-rate [Hz]" 4)
      (print-info "data-speed [byte/sec]" 4)
      (print-info "block-size [byte/(ch*qbit)]" 2)
      (print-info "qbit [bit/sample]" 2))))

(defun write-wave-header (out data-size header)
  (print-wave-header header)
  (write-sequence (string-to-octets "RIFF") out)
  (write-integer (+ 12 16 8 data-size) out :bytes 4)
  (write-sequence (string-to-octets "WAVE") out)
  (write-sequence (string-to-octets "fmt ") out)
  (write-integer 16 out :bytes 4)
  ;; fmt size
  (write-sequence header out))

(defun write-wave-data (out data-size data)
  (write-sequence (string-to-octets "data") out)
  (write-integer data-size out :bytes 4)
  (write-sequence data out))

(defun write-wave (f in)
  (flet ((get-data (file-info n)
           (let ((pos (car (svref file-info n)))
                 (size (cdr (svref file-info n))))
             (file-position in pos)
             (read-bytes in size))))
    (let ((fname (concatenate 'string (svref f 0) +wave-file-extention+))
          (data-size (cdr (svref f 3))))
      (format t "extracting ~S...~%" fname)
      (format t "  file size: ~S~%" (svref f 1))
      (with-open-file (out fname
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
        (write-wave-header out data-size (get-data f 2))
        (write-wave-data out data-size (get-data f 3))))))

;; lsd->wave*
(defun write-waves (lsdpath)
  (with-open-file (in lsdpath :direction :input :element-type '(unsigned-byte 8))
    (if (string= "LSD" (read-bytes-as-string in +magic+))
        (let ((files (cdr (nth 1 (parse-lsd in)))))
          (loop for f in files
             do (write-wave f in)))
        ;; (vector name head-bytes data-bytes)
        (format t "~s is not lsd file.~%" lsdpath))))


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
