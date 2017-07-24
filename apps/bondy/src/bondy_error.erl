%% =============================================================================
%%  bondy_error.erl -
%% 
%%  Copyright (c) 2016-2017 Ngineo Limited t/a Leapsight. All rights reserved.
%% 
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================


-module(bondy_error).
-include("bondy.hrl").
-include_lib("wamp/include/wamp.hrl").

-export([map/1]).
-export([map/2]).
-export([code_to_uri/1]).
-export([message/4]).
-export([message/5]).
-export([message/6]).




%% =============================================================================
%% API
%% =============================================================================


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
message(
    Type, ReqId, Details, #{code := C, message := _, description := _} = Map) ->
        wamp_message:error(Type, ReqId, Details, code_to_uri(C), [Map]);

message(Type, ReqId, Details, Uri) when is_binary(Uri) ->
        wamp_message:error(Type, ReqId, Details, Uri);

message(Type, ReqId, Details, Other) ->
        wamp_message:error(Type, ReqId, Details, map(Other)).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
message(
    Type, ReqId, Details, Uri, Args) ->
        wamp_message:error(Type, ReqId, Details, Uri, Args).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
message(
    Type, ReqId, Details, Uri, Args, ArgsKw) ->
        wamp_message:error(Type, ReqId, Details, Uri, Args, ArgsKw).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
code_to_uri(Reason) when is_atom(Reason) ->
    R = list_to_binary(atom_to_list(Reason)),
    <<"com.leapsight.bondy.error.", R/binary>>;

code_to_uri(Reason) when is_binary(Reason) ->
    <<"com.leapsight.bondy.error.", Reason/binary>>.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
map(#{code := _} = M) ->
    bondy_utils:to_binary_keys(M);


map(#error{arguments = undefined, arguments_kw = undefined} = Err) ->
    #error{error_uri = Uri} = Err,
    #{
        <<"code">> => Uri,
        <<"message">> => <<>>,
        <<"description">> => <<>>
    };



map(#error{arguments = L, arguments_kw = undefined} = Err)
when is_list(L) ->
    #error{error_uri = Uri} = Err,
    Mssg = case L of
        [] ->
            [];
        _ ->
            hd(L)
    end,
    #{
        <<"code">> => Uri,
        <<"message">> => Mssg,
        <<"description">> => Mssg
    };

map(#error{arguments = undefined, arguments_kw = M} = Err)
when is_map(M) ->
    #error{error_uri = Uri} = Err,
    Mssg = maps:get(<<"message">>, M),
    Desc = maps:get(<<"description">>, M, Mssg),
    #{
        <<"code">> => Uri,
        <<"message">> => Mssg,
        <<"description">> => Desc
    };

map(#error{arguments = L, arguments_kw = M} = Err) ->
    #error{error_uri = Uri} = Err,
    Mssg = case L of
        [] ->
            [];
        _ ->
            hd(L)
    end,
    Desc = maps:get(<<"description">>, M, Mssg),
    #{
        <<"code">> => Uri,
        <<"message">> => Mssg,
        <<"description">> => Desc
    };

map(oauth2_invalid_request) ->
    #{
        <<"code">> => <<"invalid_request">>,
        <<"status_code">> => 400,
        <<"message">> => <<"The request is malformed.">>,
        <<"description">> => <<"The request is missing a required parameter, includes an unsupported parameter value (other than grant type), repeats a parameter, includes multiple credentials, utilizes more than one mechanism for authenticating the client, or is otherwise malformed.">>
    };

map(oauth2_invalid_client) ->
    %% Client authentication failed (e.g., unknown client, no
    %% client authentication included, or unsupported
    %% authentication method).  The authorization server MAY
    %% return an HTTP 401 (Unauthorized) status code to indicate
    %% which HTTP authentication schemes are supported.  If the
    %% client attempted to authenticate via the "Authorization"
    %% request header field, the authorization server MUST
    %% respond with an HTTP 401 (Unauthorized) status code and
    %% include the "WWW-Authenticate" response header field
    %% matching the authentication scheme used by the client.
    #{
        <<"code">> => <<"invalid_client">>,
        <<"status_code">> => 401,
        <<"message">> => <<"Unknown client or unsupported authentication method.">>,
        <<"description">> => <<"Client authentication failed (e.g., unknown client, no client authentication included, or unsupported authentication method).">>
    };

map(oauth2_invalid_grant) ->
    #{
        <<"code">> => <<"invalid_grant">>,
        <<"status_code">> => 400,
        <<"message">> => <<"The access or refresh token provided is expired, revoked, malformed, or invalid.">>,
        <<"description">> => <<"The provided authorization grant (e.g., authorization code, resource owner credentials) or refresh token is invalid, expired, revoked, does wamp_ match the redirection URI used in the authorization request, or was issued to another client. The client MAY request a new access token and retry the protected resource request.">>
    };

map(oauth2_unauthorized_client) ->
    #{
        <<"code">> => <<"unauthorized_client">>,
        <<"status_code">> => 400,
        <<"message">> => <<"The authenticated client is not authorized to use this authorization grant type.">>,
        <<"description">> => <<>>
    };

map(oauth2_unsupported_grant_type) ->
    #{
        <<"code">> => <<"unsupported_grant_type">>,
        <<"status_code">> => 400,
        <<"message">> => <<"The requested scope is invalid, unknown, malformed, or exceeds the scope granted by the resource owner.">>,
        <<"description">> => <<>>
    };

map(oauth2_invalid_scope) ->
    #{
        <<"code">> => <<"invalid_scope">>,
        <<"status_code">> => 400,
        <<"message">> => <<"The authorization grant type is not supported by the authorization server.">>,
        <<"description">> => <<"The authorization grant type is not supported by the authorization server.">>
    };

map(invalid_scheme) ->
    Msg = <<"The authorization scheme is missing or the one provided is not the one required.">>,
    maps:put(<<"status_code">>, Msg, map(oauth2_invalid_client));

map({badarg, {decoding, json}}) ->
    #{
        <<"code">> => <<"invalid_data">>,
        <<"message">> => <<"The data provided is not a valid json.">>,
        <<"description">> => <<"Make sure the data type you are sending matches a supported mime type and that it matches the request content-type header.">>
    };

map({badarg, {decoding, msgpack}}) ->
    #{
        <<"code">> => <<"invalid_data">>,
        <<"message">> => <<"The data provided is not a valid msgpack.">>,
        <<"description">> => <<"Make sure the data type you are sending matches a supported mime type and that it matches the request content-type header.">>
    };

map({Code, Mssg}) ->
    #{
        <<"code">> => Code,
        <<"message">> => Mssg,
        <<"description">> => <<>>
    };

map({Code, Mssg, Desc}) ->
    #{
        <<"code">> => Code,
        <<"message">> => Mssg,
        <<"description">> => Desc
    };

map(Code) ->
    #{
        <<"code">> => Code
    }.


map(Uri, Term) when is_binary(Uri) ->
    maps:put(code, Uri, map(Term)).

