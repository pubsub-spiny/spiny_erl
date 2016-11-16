%%%-------------------------------------------------------------------
%% @doc spiny_erl public API
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, join/1, state/0, publish/2, subscribe/2, unsubscribe/2, is_running/0]).

-define(APP, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	io:format("Hello~n"),
    %application:start(cowboy),
	spiny_protocol:start(),
    spiny_erl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

join(Node) ->
	spiny_erl_vnode:join(Node).

state() ->
	spiny_erl_vnode:call(state).

notify(Msg) ->
	Nodes = spiny_erl_gossip:nodes(),
	spiny_erl_notifier:call({notify, Msg}, Nodes).

publish(Topic, Msg) ->
	case spiny_erl_rendezvous:en(Topic) of
		{ok, Node} ->
		    spiny_erl_publisher:call({publish, Topic, Msg}, Node);
		_ ->
			{error, node_not_found}
	end.

subscribe(Topic, Pid) ->
	spiny_erl_local_sub_man:call({subscribe, Topic, Pid}),
	case spiny_erl_rendezvous:sn(Topic) of
		{ok, Node} ->
			spiny_erl_sub_man:call({subscribe, Topic, node()}, Node);
		_ ->
			{error, node_not_found}
	end.

unsubscribe(Topic, Pid) ->
	spiny_erl_local_sub_man:call({unsubscribe, Topic, Pid}).
	%0になったらremoteからもunsubscribeする

%% @doc Is running?
is_running() ->
	io:format("~p", [erlang:whereis(spiny_erl_sup)]),
	case erlang:whereis(spiny_erl_sup) of
		undefined ->
			false;
		P when is_pid(P) ->
			true
	end.

%%====================================================================
%% Internal functions
%%====================================================================
