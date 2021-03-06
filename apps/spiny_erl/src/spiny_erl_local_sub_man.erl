%%%-------------------------------------------------------------------
%% @doc spiny_erl_local_sub_man public API
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_local_sub_man).

-behaviour(gen_server).


%% API
-export([start_link/0, start_link/1, call/1, call/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TBL, spiny_erl_local_sub_man_tbl).

% TODO: Change successor to list of successors
-record(state, {key, predecessor=nil, finger_table=[], successor=node()}).


%%====================================================================
%% API
%%====================================================================



%%--------------------------------------------------------------------
%% @doc
%% Starts the node as a standalone node
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() -> start_link([]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the node and attempts to place itself in the correct
%% location on the ring according to the known nodes.
%%
%% @spec start_link(KnownNodes) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
    
%%--------------------------------------------------------------------
%% @doc
%% Shortcut for gen_server:call(gen_chord, Request)
%%
%% @spec call(Request::term()) -> Reply::term()
%% @end
%%--------------------------------------------------------------------
call(Request) ->
	io:format("spiny_erl_local_sub_man:call(~p)~n", [Request]),
    gen_server:call(?SERVER, Request).
    
%%--------------------------------------------------------------------
%% @doc
%% Shortcut for gen_server:call({gen_chord, node()}, Request)
%%
%% @spec call(Request::term(), node()) -> Reply::term()
%% @end
%%--------------------------------------------------------------------
call(Request, Node) ->
	io:format("Calling ~p with message:~p~n", [Node, Request]),
    net_kernel:connect_node(Node),
    gen_server:call({?SERVER, Node}, Request).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
	io:format("Starting local subscribe manager...~n"),
    DefaultETSOpts = [public, named_table,
                      {read_concurrency, true}],
    ets:new(?TBL, [{keypos, 1}|DefaultETSOpts]),
    {ok, #state{}}.

    
% subscribe
handle_call({subscribe, Topic, Node}, _From, State) ->
    Reply = spiny_sub_util:add_topic(?TBL, Topic, Node),
    {reply, Reply, State};

handle_call({unsubscribe, Topic, Node}, _From, State) ->
    Reply = spiny_sub_util:del_topic(?TBL, Topic, Node),
    {reply, Reply, State};

handle_call({subscribers, Topic}, _From, State) ->
    Reply = spiny_sub_util:match(?TBL, Topic),
    {reply, Reply, State};
    
% Unkown Call
handle_call(Request, _From, State) ->
    Reply = {error, unknown_call},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

