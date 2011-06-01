;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the GNU LLGPL 2.1 (same as CCL itself)       ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2011 ITA Software, Inc.  All rights reserved. ;;;
;;;                                                                  ;;;
;;; Original authors: Francois-Rene Rideau                           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
This is a simple patch to allow Clozure CL (CCL) to run in single-threaded mode.

It was last tested to run with this checkout of CCL, revision 13568 (same as 13679).
	http://svn.clozure.com/publicsvn/openmcl/branches/qres

This is free software, available under the same license as CCL.

Note: This was submitted for inclusion in the upstream CCL distribution.
However, Gary Byers is unwilling to include and maintain this patch.

To create a single-threaded ccl, you may:
	${CCL_DEFAULT_DIRECTORY}/lx86cl64 --load make-single-threaded-ccl
To test that indeed you can run code single-threaded mode, you may:
	./single-threaded-ccl --eval '(progn (format t "~S" ccl::*application*) (ccl::show-processes) (ccl:quit))'

Note that to be able to use many of the dynamically loaded features of CCL,
you will need to either put your single-threaded-ccl in the same directory
as the rest of CCL and use it from there, or you will need to
	export CCL_DEFAULT_DIRECTORY=/path/to/ccl
|#

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

(in-package :cl-user)

;;(save-application "single-threaded-ccl" :toplevel-function #'single-threaded-toplevel :prepend-kernel t)

(save-application "single-threaded-ccl" :prepend-kernel t)
