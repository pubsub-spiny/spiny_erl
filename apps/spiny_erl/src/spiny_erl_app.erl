%%%-------------------------------------------------------------------
%% @doc spiny_erl public API
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, join/1, is_running/1]).

-define(APP, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	io:format("Hello~n"),
    spiny_erl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

join(Node) ->
	ok.

%% @doc Is running?
-spec(is_running(node()) -> boolean()).
is_running(Node) ->
	true.

%%====================================================================
%% Internal functions
%%====================================================================
