(defmodule player_actor
  (behaviour gen_server)

  (export (start_link 0) (init 1)
          (handle_call 3) (handle_cast 2) (handle_info 2)
          (code_change 3)
          (terminate 2)
         )
 )

(include-lib "_build/default/lib/wamp/include/wamp.hrl")

(defrecord state
  bondy_ref
  exchange_ref
  updated_specs
  subscriptions
 )

(defun start_link ()
  (gen_server:start-link (tuple 'local 'player_actor) 'player_actor () ())
 )

(defun init (_)
  (let* ((session-id (: bondy_session_id new))
         (ref (: bondy_ref new 'internal (self) session-id))
         ((tuple 'ok id) (: bondy_broker subscribe
                            "com.myapp.hello"
                            (map 'subscription_id (: bondy_utils gen_message_id 'global)
                                 'match (binary "exact"))
                            "keypress"
                            (make-state bondy_ref ref)))
         (state (make-state subscriptions (map id "keypress")))
        )
    (tuple 'ok state)
   )
 )

;; request from state
(defun handle_call (_ _ state)
  (tuple 'noreply state))

;; message state
(defun handle_cast (_ state)
  (tuple 'noreply state))

;; info state
(defun handle_info
  (((tuple "$bondy_request" _ "com.myapp.hello" (= event (make-event))) state)
   ;; subscription message
   (let* ((id (event-subscription_id event))
          (topic (: maps get id (state-subscriptions state) 'undefined))
          (new-state (case (tuple topic (event-args event))
                       ((tuple 'undefined _)
                        state
                        )
                       ((tuple "keypress" key)
                        ;; TODO add key into state here
                        (: io format "key pressed ~p" (list key))
                        state
                        )
                       )
            )
          )
     (tuple 'noreply new-state)
     )
   )

  (((tuple _ _ _ (= event (make-event))) state)
   (: io format "unhandled event in LFE")
   (tuple 'noreply state)
  )
  
  ((_ state)
   (: io format "unhandled info in LFE")
   (tuple 'noreply state)
  )
 )

;; reason state
(defun terminate (_ _)
  'ok
 )

;; old-version state extra
(defun code_change (_ state _)
  (tuple 'ok state)
 )