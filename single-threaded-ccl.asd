;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software under the GNU LLGPL 2.1 (same as CCL itself)       ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2008-2011 ITA Software, Inc.  All rights reserved. ;;;
;;; Copyright (c) 2011-2014 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original authors: Francois-Rene Rideau                           ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "single-threaded-ccl"
  :version "1.0.0"
  :description "Create a single-threaded CCL image, so you can fork"
  :author "Francois-Rene Rideau"
  :license "LLGPL 2.1"
  :components ((:file "single-threaded-ccl"))
  :if-feature :clozure)
