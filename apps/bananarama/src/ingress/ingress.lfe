(defmodule ingress
	(export
	; (start-link 0)
   (init 1)
   (handle-call 3)
   (handle-cast 2)
   (handle-info 2)
   (terminate 2)
   (code-change 3)
	 (on-keypress 1)
	)

	(callback
	 (valid 1)
	 (start-link 0)
	 (on-keypress 1)
	; (init 1)
	;	(filter 1)
	 )
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

;; (defun filter (request)
;; 	"Filters a message by modifying it on the way in and returning the modified message. Defaults to returning the message untouched."
;; 	request
;;  )

(defun on-keypress (_)
	(: io format "on-keypress")
 )


(defun init (_)
	(let* ((session-id (: bondy_session_id new))
				 (bondy-ref (: bondy_ref new 'internal
											 (tuple (MODULE) 'on-keypress)
											 session-id
											)
				  )
				 (options (map 'match (binary "exact")
											 'callback_args (list)))
				 (realm (binary "chat"))
				 (procedure (binary "test-rpc"))
				 ((tuple 'ok id) (: bondy_dealer register procedure options realm bondy-ref))
	
         ;; (state (make-state bondy-ref bondy-ref
				 ;; 										subscriptions (map id (binary "keypress"))
				 ;; 										players (map)
				 ;; 				           )
				 ;; 	 )
			   )
		(: erlang send_after 16 (self) 'tick)
	  (: io format "started up chat_broker with id ~p" (list id))
    (tuple 'ok (list))
	 )
 )


;; register_procedures(Session) ->

;;     %% wamp.session.{ID}.get
;;     %% -------------------------------------------------------------------------
;;     %% The wamp.session.get implementation forwards the call to this dynamic
;;     %% URI. This is required because sessions are not replicated, so we need a
;;     %% way to located the node where the session lives to route the call to it.
;;     %% If we have more session methods then we should implement prefix
;;     %% registration i.e. wamp.session.{ID}.*
;;     SessionId = bondy_session:id(Session),
;;     Extid = bondy_session_id:to_external(SessionId),
;;     Part = bondy_utils:session_id_to_uri_part(Extid),

;;     ProcUri = <<"wamp.session.", Part/binary, ".get">>,

;;     %% Notice we are implementing this as callback reference,
;;     %% this means a different reference per callback. In case we needed to
;;     %% support many more callbacks we would be better of using the session
;;     %% manager process as target, having a single reference for all procedures,
;;     %% reducing memory consumption.
;;     RealmUri = bondy_session:realm_uri(Session),
;;     MF = {bondy_session_api, get},
;;     Ref = bondy_ref:new(internal, MF, SessionId),

;;     Args = [SessionId],
;;     Opts = #{match => ?EXACT_MATCH, callback_args => Args},
;;     {ok, _} = bondy_dealer:register(ProcUri, Opts, RealmUri, Ref),

;;     ok.

(defun handle-call (_ _ state)
  (tuple 'reply 'ok state)
 )

(defun handle-cast
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

(defun handle-info
  (('tick state)
		 (: erlang send_after 16 (self) 'tick)
		 
     (tuple 'noreply state)
	)
  ((_ state)
   (tuple 'noreply state)
	)
 )

(defun terminate (_ _)
  'ok)

(defun code-change (_ state _)
  (tuple 'ok state))