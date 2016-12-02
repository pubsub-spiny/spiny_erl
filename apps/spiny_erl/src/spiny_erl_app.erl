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
    SupResult = spiny_erl_sup:start_link(),
    io:format("~p~n", [SupResult]),
    io:format("A~n", []),
    wait_for_application(spiny_erl_sup),
    io:format("B~n", []),
    wait_for_application(spiny_erl_vnode_sup),
    io:format("C~n", []),
    A = spiny_erl_vnode_man:start_vnode(),
    B = spiny_erl_vnode_man:start_vnode(),
    %C = spiny_erl_vnode_man:start_vnode(),
    io:format("A,B= ~p~p~n", [A,B]),
    SupResult.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

join(Node) ->
	spiny_erl_vnode_man:join(Node).

state() ->
	spiny_erl_vnode_man:call_vnode(state).

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
			spiny_erl_sub_man:call({subscribe, Topic, node()}, Node),
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

unsubscribe(Topic, Pid) ->
	spiny_erl_local_sub_man:call({unsubscribe, Topic, Pid}).
	%0になったらremoteからもunsubscribeする

wait_for_application(App) ->
    case is_running(App) of
        true ->
            ok;
        false ->
            timer:sleep(500),
            wait_for_application(App)
    end.

%% @doc Is running?
is_running() ->
	is_running(spiny_erl_sup).

is_running(App) ->
	io:format("~p~p~n", [node(), erlang:whereis(App)]),
	case erlang:whereis(App) of
		undefined ->
			false;
		P when is_pid(P) ->
			true
	end.

%%====================================================================
%% Internal functions
%%====================================================================
