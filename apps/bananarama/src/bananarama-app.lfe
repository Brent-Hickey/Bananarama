(defmodule bananarama-app
  (behaviour application)
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
  (logger:info "Starting apps/bananarama application ...")
  (apps/bananarama-sup:start_link))

(defun stop ()
  (apps/bananarama-sup:stop)
  'ok)

(defun stop (_)
'ok
	)