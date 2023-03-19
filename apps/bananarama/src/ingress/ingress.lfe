(defmodule ingress
	(export
	; (start-link 1)
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)
	 (on-keypress 1)
	 (test-callback 0)
	)

	(callback
	 (test-callback 1)
	 (valid? 1)
	 (on-keypress 1)
	; (init 1)
	;	(filter 1)
	 )
	)

(defrecord state
	bondy_ref
	message
 )

(defrecord request
		type payload
 )
;;	"Is the incoming request valid? Defaults to true."

(defun valid?
	(((match-request))
	 'true
	 )
	((_)
	 'false
	 )
	)

(defun test-callback ()
	(: io format "test-callback in ingress")
 )

;; (defun filter (request)
;; 	"Filters a message by modifying it on the way in and returning the modified message. Defaults to returning the message untouched."
;; 	request
;;  )

(defun on-keypress (_)
	;;(: module test-callback)
	;;(: io format "on-keypress handled by module ~p~n" (list module))
	(: io format "on-keypress handled by module ~n")
	(tuple 'ok (map) (list) (map))
 )

(defun init (module)
	(let* ((session-id (: bondy_session_id new))
				 (rpc-bondy-ref (: bondy_ref new 'internal
																(tuple (MODULE) 'on-keypress)
																session-id
																)
				   )
				 (bondy-ref (: bondy_ref new 'internal
											 (self)
											 session-id
											)
				  )
				 (options (map 'match (binary "exact")
											 'callback_args (list)))
				 (realm (binary "chat"))
				 (procedure (binary "test-rpc"))
				 ((tuple 'ok rpc-id) (: bondy_dealer register procedure options realm rpc-bondy-ref))
				 
	
         (state (make-state bondy_ref bondy-ref
														message ""
								           )
					 )
				 )
				 (: erlang send_after 1000 (self) 'tick)
	  (: io format "started up ingress for module: ~p with id ~p~n" (list module rpc-id))
    (tuple 'ok state)
	 )
 )
	

(defun handle_call (_ _ state)
  (tuple 'reply 'ok state)
 )

(defun handle_cast
	(((= (match-request) request) state)
		 (if (valid? request)
				; (: gen-server cast (destination request) (filter request))
				 (io:format "Unhandled request: ~p~n" (list request))
				)
	  state
	 )
	((invalid-request state)
	 (io:format "Invalid request: ~p~n" (list invalid-request))
	  state
	 )
 )

(defun handle_info
  (('tick state)
	 (: erlang send_after 1000 (self) 'tick)
	 (: bondy_broker publish
			(: bondy_utils gen_message_id 'global)
			(map)
			(tuple (binary "chat") (binary "message-update"))
			(list)
			(map)
			(: bondy_context local_context (binary "chat") (state-bondy_ref state))
		 )
     (tuple 'noreply state)
	)
  ((_ state)
   (tuple 'noreply state)
	)
 )

(defun terminate (_ _)
  'ok)

(defun code_change (_ state _)
  (tuple 'ok state))