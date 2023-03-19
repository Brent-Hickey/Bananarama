(defmodule region-sup
	(behavior supervisor)
	
  (export
	 (start_link 0)
	 (init 1)
	 )
 )

(defun start_link ()
	;gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	(: supervisor start_link (tuple 'local (MODULE)) (MODULE) (list))
 )

(defun init (_) ;; args
	(let ((sup-flags (map 'strategy 'one_for_one
												'intensity 3
												'period 60)
				 )
				(child-specs (list
											(map 'id 'chat-ingress
													 'start (tuple 'chat-ingress 'start-link (list))
											 		 'restart 'permanent
											 		 'shutdown 2000
											 		 'type 'worker
													 )
											(map 'id 'chunk-state
													 'start (tuple 'chunk-state 'start-link (list))
											 		 'restart 'permanent
											 		 'shutdown 2000
											 		 'type 'worker
													 )
											;;	(map 'id chat-mon)
											
											;; (map 'id 'chat-broker
											;; 					'start (tuple 'chat-broker 'start_link (list))
											;; 					'restart 'permanent
											;; 					'shutdown 2000
											;; 					'type 'worker
											;; 					'modules (list 'chat-broker)
											;; 		     )
													 ;; (map 'id 'broadcaster
													 ;; 			'start (tuple 'broadcaster 'start_link (list))
													 ;; 			'restart 'permanent
													 ;; 			'shutdown 2000
													 ;; 			'type 'worker
													 ;; 			'modules (list 'broadcaster)
													 ;; 		 )
											  )
				  )
				)
		(tuple 'ok (tuple sup-flags child-specs))
   )
 )