%% -----------------------------------------------------------------------------
%% @doc A `bondy_ref' (reference) acts as a fully qualified name for a
%% process or callback function in a Bondy network. The reference is used by
%% Bondy in the `bondy_registry' when registering procedures and subscriptions
%% so that the `bondy_router' can forward and/or relay a message to a process,
%% or callback function.
%%
%% ## Types
%% ### Internal
%%
%% ### Relay
%%
%% ### Client
%%
%% ### Callback
%%
%% @since 0.1.0
%% @end
%% -----------------------------------------------------------------------------
-module(bondy_ref).

-include_lib("wamp/include/wamp.hrl").
-include("bondy.hrl").

-define(TYPES, [relay, bridge_relay, internal, client]).

%% The order of the record fields is defined so that the ref can be used as a
%% key in an ordered_set store supporting prefix mapping, so do not change it
%% unless you know exactly what you are doing.
%% Changing this order might affect the following modules:
%% - bondy_registry_entry
%% - bondy_registry
%% - bondy_rpc_promise
%% - bondy_session
%% TODO consider reeimplementing this as
% -record(bondy_ref, {
%     %% <<"bondy:ref:{type}:{realm}:{node}:{random}:{target_type}:{target_handle}">>
%     id                  ::  binary(),
%     %% WAMP session id integer
%     session_id          ::  wildcard(maybe(id())),
%     target_type         ::  wildcard(target_type()),
%     target_handle       ::  binary() | term | mf()
% }).

-record(bondy_ref, {
    realm_uri           ::  uri(),
    nodestring          ::  wildcard(nodestring()),
    session_id          ::  wildcard(maybe(id())),
    %% TODO split target into target_type and target_handle
    target              ::  wildcard(target()),
    type                ::  wildcard(ref_type())
}).

-type t()               ::  #bondy_ref{}.
-type relay()           ::  #bondy_ref{type :: relay}.
-type bridge_relay()    ::  #bondy_ref{type :: bridge_relay}.
-type client()          ::  #bondy_ref{type :: client}.
-type internal()        ::  #bondy_ref{type :: internal}.
-type ref_type()        ::  relay | bridge_relay | internal | client.
-type target()          ::  {pid, binary()}
                            | {name, term()}
                            | {callback, mf()}.
-type target_type()     ::  pid | name | callback.
-type name()            ::  term().
-type mf()              ::  {M :: module(), F :: atom()}.
-type wildcard(T)       ::  T | '_'.

-export_type([t/0]).
-export_type([relay/0]).
-export_type([bridge_relay/0]).
-export_type([client/0]).
-export_type([internal/0]).
-export_type([mf/0]).
-export_type([target/0]).

-export([callback/1]).
-export([is_relay/1]).
-export([is_bridge_relay/1]).
-export([is_client/1]).
-export([is_internal/1]).
-export([is_local/1]).
-export([is_local/2]).
-export([is_self/1]).
-export([is_type/1]).
-export([types/0]).
-export([name/1]).
-export([new/2]).
-export([new/3]).
-export([new/4]).
-export([new/5]).
-export([node/1]).
-export([nodestring/1]).
-export([pattern/5]).
-export([pid/1]).
-export([realm_uri/1]).
-export([session_id/1]).
-export([target/1]).
-export([target_type/1]).
-export([type/1]).



%% =============================================================================
%% API
%% =============================================================================



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec new(Type :: ref_type(), RealmUri :: uri()) ->
    t().

new(Type, RealmUri) ->
    new(Type, RealmUri, self()).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec new(
    Type :: ref_type(),
    RealmUri :: uri(),
    Target :: pid() | mf() | name()) ->
    t().

new(Type, RealmUri, Target) ->
    new(Type, RealmUri, Target, undefined).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec new(
    Type :: ref_type(),
    RealmUri :: uri(),
    Target :: pid() | mf() | name(),
    SessionId :: maybe(bondy_session:id())) -> t().

new(Type, RealmUri, Target, SessionId) ->
    Node = bondy_config:nodestring(),
    new(Type, RealmUri, Target, SessionId, Node).


%% -----------------------------------------------------------------------------
%% @doc Creates a new reference.
%% @end
%% -----------------------------------------------------------------------------
-spec new(
    Type :: ref_type(),
    RealmUri :: uri(),
    Target :: pid() | mf() | name(),
    SessionId :: maybe(bondy_session:id()),
    Node :: node() | nodestring()) -> t().

new(Type, RealmUri, Target, SessionId, Node) when is_atom(Node) ->
    new(Type, RealmUri, Target, SessionId, atom_to_binary(Node, utf8));

new(Type, RealmUri, Target0, SessionId, Nodestring)
when is_binary(RealmUri), is_binary(Nodestring) ->

    is_integer(SessionId)
        orelse SessionId == undefined
        orelse error({badarg, {session_id, SessionId}}),

    lists:member(Type, ?TYPES)
        orelse error({badarg, {type, Type}}),

    Target = validate_target(Target0),

    #bondy_ref{
        type = Type,
        realm_uri = RealmUri,
        nodestring = Nodestring,
        session_id = SessionId,
        target = Target
    }.



%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec pattern(
    Type :: wildcard(ref_type()),
    RealmUri :: uri(),
    Target :: wildcard(pid() | mf() | name()),
    SessionId :: wildcard(maybe(bondy_session:id())),
    Node :: wildcard(node() | nodestring())) -> t().


pattern(Type, RealmUri, Target0, SessionId, Node)
when is_binary(RealmUri) ->

    Nodestring = case Node of
        '_' ->
            Node;
        Node when is_atom(Node) ->
            atom_to_binary(Node, utf8);
        Node when is_binary(Node) ->
            Node;
        _ ->
            error({badarg, {node, Node}})
    end,

    lists:member(Type, ?TYPES ++ ['_'])
        orelse error({badarg, {type, Type}}),

    Target = validate_target(Target0, _AllowPattern = true),

    is_integer(SessionId)
        orelse SessionId == '_'
        orelse error({badarg, {session_id, SessionId}}),

    #bondy_ref{
        type = Type,
        realm_uri = RealmUri,
        nodestring = Nodestring,
        session_id = SessionId,
        target = Target
    }.


%% -----------------------------------------------------------------------------
%% @doc Returns the reference type.
%% @end
%% -----------------------------------------------------------------------------
-spec type(Term :: term()) -> ref_type().

type(#bondy_ref{type = Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc Returns all the supported reference types.
%% @end
%% -----------------------------------------------------------------------------
-spec types() -> [ref_type()].

types() ->
    ?TYPES.


%% -----------------------------------------------------------------------------
%% @doc Returns the realm URI of the reference.
%% @end
%% -----------------------------------------------------------------------------
-spec realm_uri(t()) -> uri().

realm_uri(#bondy_ref{realm_uri = Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc Returns the Bondy peer binary string name of the node in which the
%% target of this reference is located and/or connected to.
%%
%% See {@link target/1} for a description of the different targets and the
%% relationship with the node.
%% @end
%% -----------------------------------------------------------------------------
-spec nodestring(t()) -> nodestring().

nodestring(#bondy_ref{nodestring = Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc Returns the Bondy peer node in which the target of this reference is
%% located and/or connected to.
%%
%% See {@link target/1} for a description of the different targets and the
%% relationship with the node.
%% @end
%% -----------------------------------------------------------------------------
-spec node(t()) -> node().

node(#bondy_ref{nodestring = Val}) ->
    binary_to_atom(Val, utf8).


%% -----------------------------------------------------------------------------
%% @doc Returns the session identifier of the reference or the atom `undefined'.
%% @end
%% -----------------------------------------------------------------------------
-spec session_id(t()) -> maybe(id()).

session_id(#bondy_ref{session_id = Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_relay(t()) -> boolean().

is_relay(Ref) ->
    relay =:= type(Ref).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_bridge_relay(t()) -> boolean().

is_bridge_relay(Ref) ->
    bridge_relay =:= type(Ref).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_client(t()) -> boolean().

is_client(Ref) ->
    client =:= type(Ref).


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec is_internal(t()) -> boolean().

is_internal(Ref) ->
    internal =:= type(Ref).


%% -----------------------------------------------------------------------------
%% @doc Returns whether this is a reference to a local target i.e. located on
%% the caller's node.
%% @end
%% -----------------------------------------------------------------------------
-spec is_local(Ref :: t()) -> boolean().

is_local(#bondy_ref{nodestring = Val}) ->
    Val =:= bondy_config:nodestring().


%% -----------------------------------------------------------------------------
%% @doc Returns whether this is a reference to a target local to the node
%% represented by `Nodestring'.
%% @end
%% -----------------------------------------------------------------------------
-spec is_local(Ref :: t(), nodestring()) -> boolean().

is_local(#bondy_ref{nodestring = Val}, Nodestring) ->
    Val =:= Nodestring.


%% -----------------------------------------------------------------------------
%% @doc Returns whether this is a reference to the calling process.
%% @end
%% -----------------------------------------------------------------------------
-spec is_self(Ref :: t()) -> boolean().

is_self(#bondy_ref{} = Ref) ->
    (catch pid(Ref)) =:= self().


%% -----------------------------------------------------------------------------
%% @doc Returns `true' if term `Term' is a reference. Otherwise returns `false'.
%% @end
%% -----------------------------------------------------------------------------
-spec is_type(t()) -> boolean().

is_type(#bondy_ref{}) ->
    true;

is_type(_) ->
    false.


%% -----------------------------------------------------------------------------
%% @doc Returns the target of the reference. A target is a process (`pid()') a
%% gproc registered name (`{name, GprocName}') or a callback (`mf()').
%%
%% If the reference type is `client' the target refers to the
%% session and/or process owning the network connection to the client.
%%
%% If the reference type is `internal' the target refers to an internal
%% process acting as one of the Bondy client roles e.g. an internal subscriber.
%% Messages destined to a target located on a different node are relayed by the
%% router through the cluster distribution layer (Partisan).
%%
%% If the reference type is `relay' the target refers to the session
%% and/or process owning the network connection to the edge or
%% remote cluster (See {@link bondy_edge_uplink_client} and
%% {@link bondy_edge_uplink_server} respectively). The relay acts as a proxy
%% for the actual edge or remote clients, internal and callback targets.
%%
%% @end
%% -----------------------------------------------------------------------------
-spec target(t()) -> target().

target(#bondy_ref{target = Val}) ->
    Val.


%% -----------------------------------------------------------------------------
%% @doc Returns the target of the reference. A target is a process (`pid()') a
%% gproc registered name (`{name, GprocName}') or a callback (`mf()').
%%
%% If the reference type is `client' the target refers to the
%% session and/or process owning the network connection to the client.
%%
%% If the reference type is `internal' the target refers to an internal
%% process acting as one of the Bondy client roles e.g. an internal subscriber.
%% Messages destined to a target located on a different node are relayed by the
%% router through the cluster distribution layer (Partisan).
%%
%% If the reference type is `relay' the target refers to the session
%% and/or process owning the network connection to the edge or
%% remote cluster (See {@link bondy_edge_uplink_client} and
%% {@link bondy_edge_uplink_server} respectively). The relay acts as a proxy
%% for the actual edge or remote clients, internal and callback targets.
%%
%% @end
%% -----------------------------------------------------------------------------
-spec target_type(t()) -> wildcard(target_type()).

target_type(#bondy_ref{target = {Type, _}}) ->
    Type;

target_type(#bondy_ref{target = '_'}) ->
    '_'.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec name(t()) -> maybe(term()).

name(#bondy_ref{target = {name, Val}}) ->
    Val;

name(#bondy_ref{}) ->
    undefined.


%% -----------------------------------------------------------------------------
%% @doc Returns a pid when target is of type `{pid, binary()}', otherwise
%% returns the atom `undefined'.
%% If the reference is not local it fails with exception `not_my_node'. This is
%% becuase process identifies can only be used on the node where they were
%% created (this is because we are using Partisan and not Distributed Erlang).
%% @end
%% -----------------------------------------------------------------------------
-spec pid(t()) -> maybe(pid()) | no_return().

pid(#bondy_ref{target = {pid, Bin}} = Ref) ->
    %% Pids can only be used on the node where they were created (this is
    %% because we are using Partisan and not Distributed Erlang).
    is_local(Ref) orelse error(not_my_node),
    list_to_pid(binary_to_list(Bin));

pid(#bondy_ref{target = {name, Name}} = Ref) ->
    %% Pids can only be used on the node where they were created (this is
    %% because we are using Partisan and not Distributed Erlang).
    %% Also we only use gproc locally, if this ref is for another node then we
    %% do not have the session here.
    is_local(Ref) orelse error(not_my_node),
    gproc:lookup_pid({n, l, Name});

pid(#bondy_ref{}) ->
    undefined.


%% -----------------------------------------------------------------------------
%% @doc
%% @end
%% -----------------------------------------------------------------------------
-spec callback(t()) -> maybe(mf()).

callback(#bondy_ref{target = {callback, Val}}) ->
    Val;

callback(#bondy_ref{}) ->
    undefined.



%% =============================================================================
%% PRIVATE
%% =============================================================================



%% @private
validate_target(Target) ->
    validate_target(Target, false).


%% @private
validate_target(Target, AllowPattern) ->
    case Target of
        undefined ->
            error({badarg, {target, Target}});

        {M, F} when is_atom(M), is_atom(F) ->
            {callback, Target};

        Pid when is_pid(Pid) ->
            {pid, list_to_binary(pid_to_list(Pid))};

        '_'  ->
            AllowPattern == true
                orelse error({badarg, {target, Target}}),
            '_';

        Term ->
            {name, Term}
    end.