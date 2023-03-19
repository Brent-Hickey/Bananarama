(defmodule chat-ingress
  (behavior ingress)

	(export
																				;(init 1)
	 (test-callback 0)
	 (start-link 0)
	 (valid? 1)
	; (filter 1)
	 ;; (destination 1)
	)	
 )

(defun start-link ()
	(: gen_server start_link (tuple 'local (MODULE)) 'ingress (MODULE) (list))
 )

(defun test-callback ()
	(: io format "test-callback in CHAT ingress")
 )

(defun valid? (_)
	 "Is the incoming request valid?"
	 ;; (and (== (: ingress request-type request) 'chat)
	 ;;   		(< (length (: ingress request-payload request)) 100)
	 ;;  	 )
	 'true
 )

;; (defun filter (request)
;; 	"Filters a message by modifying it on the way in and returning the modified message. Defaults to returning the message untouched."
;; 	request
;;  )