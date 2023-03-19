(defmodule bananarama-app
  (behavior application)
  ;; app implementation
  (export
   (start 2)
   (stop 0)
	 (stop 1))
	)

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_ _) ;; type _args
  (logger:set_application_level 'bananarama 'all)
  (logger:info "Starting bananarama application ...")
  (bananarama-sup:start_link))

(defun stop ()
  (bananarama-sup:stop)
  'ok
 )

(defun stop (_)
	'ok
 )