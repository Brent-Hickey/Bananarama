(defmodule bananarama_router
  (export
	  (admin_base_routes 0)
	)
)

(defun admin_base_routes ()
	'(#("_" (#("/ws" bondy_wamp_ws_connection_handler #())
					 #("/ping" bondy_admin_ping_http_handler #())
					 #("/ready" bondy_admin_ready_http_handler #())
					 #("/metrics/[:registry]" prometheus_cowboy2_handler, ())
					 #("/readyLFE" bananarama_admin_ready_http_handler, #())
					)
		 )
	 )
)