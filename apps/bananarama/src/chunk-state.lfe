(defmodule chunk-state
		(export all)

 )

(include-lib "_build/default/lib/wamp/include/wamp.hrl")

(defrecord state
  bondy_ref
  subscriptions
	players
	message ;; testing
	sub-id
 )

(defun start-link ()
	(: gen_server start_link (tuple 'local (MODULE)) (MODULE) (list) (list))
 )

(defun init (_)
	(let* ((session-id (: bondy_session_id new))
				 (bondy-ref (: bondy_ref new 'internal
											 (self)
											 session-id
											 )
				   )
				 (options (map 'match (binary "exact")
											 'subscription_id (: bondy_ref new 'internal (self) session-id)))
				 (realm (binary "chat"))
				 ((tuple 'ok id) (: bondy_broker subscribe realm options (binary "message-update") bondy-ref))
				 
         (state (make-state bondy_ref bondy-ref
														subscriptions (map id (binary "message-update"))
														players (map)
														sub-id id
													 )
					 )
				 )
		(: erlang send_after 1000 (self) 'tick)
		(: io format "started up chunk-state with id ~p~n" (list id))
		(tuple 'ok state)
		)	
 )

(defun handle_info
  (((tuple _ _ (binary "chat") (= event (make-event))) state)
	 (let ((message (event-args event)))
		 (: io format "appending message update: ~p into state. New message state: ~p" (list message (++ message (state-message state))))
		 (tuple 'noreply (make-state
														 bondy_ref (state-bondy_ref state)
														 subscriptions (state-subscriptions state)
														 players (state-players state)
														 message (++ message (state-message state))
														)
						)
		)
	 )
	(('tick state)
	 (: erlang send_after 1000 (self) 'tick)
	 (: bondy_broker publish
			(state-sub-id state)
			(map)
		  (binary "chunk-update")
			(list (state-message state))
			(map)
			(: bondy_context local_context (binary "chat") (state-bondy_ref state))
		 )
	 (tuple 'noreply state)
	)
	((info state)
	 (: io format "unhandled info ~p" (list info))
	 (tuple 'noreply state)
	)
 )
		
(defun handle_cast
	((invalid-request state)
	 (io:format "Invalid request: ~p~n" (list invalid-request))
	  state
	 )
 )