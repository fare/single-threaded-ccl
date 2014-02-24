":" ; exec ccl --no-init --load "$0" "$@" ; exit 42

(in-package :cl-user)

(load (merge-pathnames "single-threaded-ccl" *load-truename*))

(ccl:save-application
 "single-threaded-ccl"
 ;; :toplevel-function #'ccl::single-threaded-toplevel
 :prepend-kernel t)
