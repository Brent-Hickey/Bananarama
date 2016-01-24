-module(ramp_config).

-define(APP, ramp).

-export[automatically_create_realms/0].
-export[connection_lifetime/0].
-export[http_acceptors_pool_size/0].
-export[http_max_connections/0].
-export[http_port/0].
-export[https_acceptors_pool_size/0].
-export[https_max_connections/0].
-export[https_port/0].
-export[tcp_acceptors_pool_size/0].
-export[tcp_max_connections/0].
-export[tcp_port/0].
-export[tls_acceptors_pool_size/0].
-export[tls_max_connections/0].
-export[tls_port/0].
-export[ws_compress_enabled/0].



%% =============================================================================
%% HTTP
%% =============================================================================


http_acceptors_pool_size() ->
    u:get_integer(?APP, http_acceptors_pool_size, 200).

http_max_connections() ->
    u:get_integer(?APP, http_max_connections, 1000).

http_port() ->
    u:get_integer(?APP, http_port, 8080).


%% @doc
%% x-webkit-deflate-frame compression draft which is being used by some
%% browsers to reduce the size of data being transmitted supported by Cowboy.
%% @end
ws_compress_enabled() -> true.

%% =============================================================================
%% HTTPS
%% =============================================================================

https_acceptors_pool_size() ->
    u:get_integer(?APP, https_acceptors_pool_size, 200).

https_max_connections() ->
    u:get_integer(?APP, https_max_connections, 1000).

https_port() ->
    u:get_integer(?APP, https_port, 443).


%% =============================================================================
%% TCP
%% =============================================================================


tcp_acceptors_pool_size() ->
    u:get_integer(?APP, tcp_acceptors_pool_size, 200).

tcp_max_connections() ->
    u:get_integer(?APP, tcp_max_connections, 1000).

tcp_port() ->
    u:get_integer(?APP, tcp_port, 10082).


%% =============================================================================
%% TLS
%% =============================================================================


tls_acceptors_pool_size() ->
    u:get_integer(?APP, tls_acceptors_pool_size, 200).

tls_max_connections() ->
    u:get_integer(?APP, tls_max_connections, 1000).

tls_port() ->
    u:get_integer(?APP, tls_port, 10083).


%% =============================================================================
%% REALMS
%% =============================================================================


automatically_create_realms() ->
    u:get_boolean(?APP, automatically_create_realms, true).


%% =============================================================================
%% SESSION
%% =============================================================================

-spec connection_lifetime() -> session | connection.
connection_lifetime() ->
    u:get_atom(?APP, connection_lifetime, session).
