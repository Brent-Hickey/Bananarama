(defmodule bananarama-sup
	(behavior supervisor)
	
  (export
	 (start_link 0)
	 (init 1)
	 )
 )

(defun start_link ()
  (: supervisor start_link (tuple 'local (MODULE)) (MODULE) (list))
	)

(defun init (_) ;; args
	(let ((sup-flags (map 'strategy 'one_for_one
												'intensity 3
												'period 60)
				 )
				(child-specs (list
											;; (map 'id 'chat-broker
											;; 		 'start (tuple 'chat-broker 'start_link (list))
											;; 		 'restart 'permanent
											;; 		 'shutdown 2000
											;; 		 'type 'worker
											;; 		 'modules (list))
											(map 'id 'region-sup
													 'start (tuple 'region-sup 'start_link (list))
													 'restart 'permanent
													 'shutdown 2000
													 'type 'supervisor
													 'modules (list 'region-sup)
													)
											)
				 )
		    )
		 (tuple 'ok (tuple sup-flags child-specs))
	 )
 )