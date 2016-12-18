%%%-------------------------------------------------------------------
%% @doc spiny_erl top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    PublisherServer = {spiny_erl_publisher, {spiny_erl_publisher, start_link, []},
              Restart, Shutdown, Type, [spiny_erl_publisher]},
    SubscriberManager = {spiny_erl_sub_man, {spiny_erl_sub_man, start_link, []},
              Restart, Shutdown, Type, [spiny_erl_sub_man]},
    LocalSubscriberManager = {spiny_erl_local_sub_man, {spiny_erl_local_sub_man, start_link, []},
              Restart, Shutdown, Type, [spiny_erl_local_sub_man]},
    DeliverServer = {spiny_erl_deliver, {spiny_erl_deliver, start_link, []},
              Restart, Shutdown, Type, [spiny_erl_deliver]},
    %VnodeServer = {chord_sup, {spiny_erl_vnode_sup, start_link, []},
    %          Restart, Shutdown, supervisor, [spiny_erl_vnode_sup]},
    %VnodeManServer = {spiny_erl_vnode_man, {spiny_erl_vnode_man, start_link, []},
    %          Restart, Shutdown, Type, [spiny_erl_vnode_man]},
    {ok, {SupFlags, [PublisherServer, SubscriberManager, LocalSubscriberManager, DeliverServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
