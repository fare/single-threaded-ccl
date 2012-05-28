;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the GNU LLGPL 2.1 (same as CCL itself)       ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2011 ITA Software, Inc.  All rights reserved. ;;;
;;; Copyright (c) 2011-2012 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original authors: Francois-Rene Rideau                           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem single-threaded-ccl
  :license "LLGPL 2.1"
  :components (#+clozure (:file "single-threaded-ccl")))
