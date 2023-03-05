(defmodule bananarama_admin_ready_http_handler
	(export
	  (init 2)
	 )
 )

;; %% =============================================================================
;; %% API
;; %% =============================================================================

(defun init (Req0 State)
	(let* ((Method (: cowboy_req method Req0))
				 (Req1 (: bondy_http_utils set_meta_headers Req0))
				 (Req2 (ready Method Req1))
				)
	  (tuple 'ok Req2 State)
	 )
 )

(defun ready
	(((binary "GET") Req)
	 (let ((Status (status_code (: bondy_config get 'status 'undefined))))
		  (: cowboy_req reply Status Req)
		 )
	 )
	
	((_ Req)
	 (: cowboy_req reply 405 Req)
	 )
 )

;; original erlang
;; init(Req0, State) ->
;;     Method = cowboy_req:method(Req0),
;;     Req1 = bondy_http_utils:set_meta_headers(Req0),
;;     Req2 = ready(Method, Req1),
;;     {ok, Req2, State}.

;; ready(<<"GET">>, Req) ->
;;     Status = status_code(bondy_config:get(status, undefined)),
;;     cowboy_req:reply(Status, Req);

;; ready(_, Req) ->
;;     cowboy_req:reply(?HTTP_METHOD_NOT_ALLOWED, Req).




;%% =============================================================================
;%% PRIVATE
;%% =============================================================================

(defun status_code
	(('ready) 204)
	((_) 500)
 )

;;status_code(ready) -> ?HTTP_NO_CONTENT;
;;status_code(_) -> ?HTTP_SERVICE_UNAVAILABLE.