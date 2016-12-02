%%%-------------------------------------------------------------------
%% @doc spiny_erl_vnode_sup top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_vnode_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_vnode/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_vnode(Id, KnownVnode) -> 
    supervisor:start_child(?SERVER, [Id, KnownVnode]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    io:format("Starting vnode sup...~n"),
    % simple_one_for_oneはstart childで子プロセスを起動させるために必要です。
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    % permanent が設定されると、子プロセスは常に再起動させられます。
    % transient が設定されると、通常とは違う理由で終了したなど、異常終了時にのみ再起動させら
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    VnodeServer = {spiny_erl_vnode, {spiny_erl_vnode, start_link, []},
              Restart, Shutdown, Type, [spiny_erl_vnode]},
    {ok, {SupFlags, [VnodeServer]}}.


%%====================================================================
%% Internal functions
%%====================================================================
