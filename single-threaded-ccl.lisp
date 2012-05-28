#+xcvb (module ())

(in-package :ccl)

(defun finish-outputs ()
  (finish-output *standard-output*)
  (finish-output *error-output*)
  (finish-output *terminal-output*)
  (finish-output *trace-output*)
  (housekeeping))

(defun show-processes ()
  (let ((p (all-processes)))
    (format t "~&Total number of running lisp processes: ~d~%~W~%" (length p) p)
    (finish-outputs)))

(defun flushing-rep ()
  (print-listener-prompt *standard-output*)
  (finish-outputs)
  (let* ((eof '#.'#:eof)
         (sexp (read *standard-input* nil eof nil)))
    (when (eq sexp eof)
      (finish-outputs)
      (throw :flushing-rep-eof nil))
    ;; This use of eval is OK because it's part of the build infrastructure.
    (format t "~&~S~%" (eval sexp))
    (finish-outputs)))

(defun flushing-repl ()
  (catch :flushing-rep-eof
    (loop (flushing-rep))))

(defun single-threaded-toplevel ()
  (housekeeping)
  ;;(show-processes)
  ;;(flushing-repl)
  (listener-function)
  (quit 0))

(defclass single-threaded-lisp-development-system (lisp-development-system)
  ())

(defmethod repl-function-name ((a single-threaded-lisp-development-system))
  'single-threaded-toplevel)

(defmethod toplevel-function ((a single-threaded-lisp-development-system) init-file)
  (%set-toplevel (or (repl-function-name a) 'single-threaded-toplevel))
  (startup-ccl (and *load-lisp-init-file* init-file))
  (toplevel))

(defparameter *application* (make-instance 'single-threaded-lisp-development-system))

#|
;; Use it as follows:
(ccl::save-application
 "single-threaded-ccl"
 ;; :toplevel-function #'single-threaded-toplevel
 :prepend-kernel t)
|#
