(defmodule bananarama-server
  (behaviour gen_server)
  ;; gen_server implementation
  (export
    (start_link 0)
    (stop 0)
	 )
  ;; callback implementation
  (export
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)
	 )
  ;; server API
  (export
    (pid 0)
    (echo 1))
	 )

;; ----------------
;; config functions
;; ----------------

(defun SERVER () (MODULE))
(defun initial-state () '#())
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         (initial-state)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (state)
  `#(ok ,state))

(defun handle_cast (_ state) ; msg state
  `#(noreply ,state))

(defun handle_call
  (('stop _ state) ; from
    `#(stop shutdown ok ,state))
  ((`#(echo ,msg) _ state) ;from
    `#(reply ,msg ,state))
  ((_ _ state) ;message from
    `#(reply ,(unknown-command) ,state)))

(defun handle_info
  (((tuple 'EXIT _ 'normal) state) ;from
   (tuple 'noreply state))
  (((tuple 'EXIT pid reason) state)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   (tuple 'noreply state))
  ((_ state) ;msg
   (tuple 'noreply state)))

(defun terminate (_ _) ; _reason _state
  'ok)

(defun code_change (_ state _) ; old-version extra
  `#(ok ,state)
  )

;;; --------------
;;; our server API
;;; --------------

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))




