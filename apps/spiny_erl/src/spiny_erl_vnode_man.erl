%%%-------------------------------------------------------------------
%% @doc spiny_erl_vnode_man
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_vnode_man).

-behaviour(gen_server).

-include("spiny_erl.hrl").

%% API
-export([start_link/0, start_link/1, call/1, call/2, call_vnode/1, call_vnode/2, lookup/1, join/1, start_vnode/0, get_pid/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(TBL, spiny_erl_vnode_man_tbl).


%%%===================================================================
%%% API
%%%===================================================================

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
	io:format("spiny_erl_vnode_man:Calling with message:~p~n", [Request]),
    gen_server:call(?SERVER, Request).
    
%%--------------------------------------------------------------------
%% @doc
%% Shortcut for gen_server:call({gen_chord, node()}, Request)
%%
%% @spec call(Request::term(), node()) -> Reply::term()
%% @end
%%--------------------------------------------------------------------
call(Request, Node) ->
	io:format("spiny_erl_vnode_man:Calling ~p with message:~p~n", [Node, Request]),
    net_kernel:connect_node(Node),
    gen_server:call({?SERVER, Node}, Request).


lookup(Key) ->
    call_vnode({lookup, Key}).

join(Node) ->
    call_vnode({join, Node}).

call_vnode(Request) ->
    call_vnode(Request, #vnode{id=nil, node=node()}).

call_vnode(Request, #vnode{id=Id, node=Node}) ->
	io:format("spiny_erl_vnode_man:call_vnode(~p, #vnode{~p, ~p})~n", [Request, Id, Node]),
    %net_kernel:connect_node(Node),
    try Node=:=node() of
        true ->
            {ok, Pid} = gen_server:call(?SERVER, {get_pid, Id}),
            spiny_erl_vnode:call(Pid, Request);
        false ->
            {ok, Pid} = gen_server:call({?SERVER, Node}, {get_pid, Id}),
            spiny_erl_vnode:call(Pid, Request)
    catch
         exit:X ->
            io:format("call_vnode error ~p~n", [X])
    end;
call_vnode(Request, Pid) when is_pid(Pid) ->
    spiny_erl_vnode:call(Pid, Request).

%call_vnode(Request, Pid) when is_pid(Pid) ->
%	io:format("spiny_erl_vnode_man:call_vnode(~p, ~p)~n", [Request, Pid]),
%    spiny_erl_vnode:call(Pid, Request).

start_vnode() ->
	io:format("spiny_erl_vnode_man:start_vnode~n", []),
    Id = chord_lib:uuid(),
    case gen_server:call(?SERVER, last_node) of
        {ok, LastVnode} ->
            start_vnode_private(Id, LastVnode);
        {error, Reason} ->
            start_vnode_private(Id, nil)
    end.

start_vnode_private(Id, VNode) ->
    case spiny_erl_vnode_sup:start_vnode(Id, VNode) of
        {ok, Pid} ->
            gen_server:call(?SERVER, {start_vnode, Id, Pid});
        {error, Reason} ->
            {error, Reason}
    end.

get_pid(Id) ->
    gen_server:call(?SERVER, {get_pid, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(Args) ->
	io:format("Starting vnode man...~n"),
    DefaultETSOpts = [public, named_table,
                      {read_concurrency, true}],
    ets:new(?TBL, [{keypos, 1}|DefaultETSOpts]),
    {ok, {}}.

    
% publish
handle_call(start_vnode, _From, {}) ->
	handle_call(start_vnode, _From, {nil});

handle_call({start_vnode, Id, Pid}, _From, State) ->
    ets:insert(?TBL, {Id, Pid}),
    {reply, {ok, Pid}, {Pid}};

handle_call(last_node, _From, {}) ->
	Reply = {error, not_found},
    {reply, Reply, {}};

handle_call(last_node, _From, {LastVnode}) ->
	Reply = {ok, LastVnode},
    {reply, Reply, {LastVnode}};

handle_call({get_pid, nil}, _From, {LastVnode}) ->
    {reply, {ok, LastVnode}, {LastVnode}};
handle_call({get_pid, Id}, _From, State) ->
    io:format("get_pid ~p~n", [Id]),
    Reply = case ets:lookup(?TBL, Id) of
    	[{Id, Pid}] ->
    		{ok, Pid};
    	[] ->
    		{error, not_found}
    end,
    io:format("get_pid Reply~p~n", [Reply]),
    {reply, Reply, State};


handle_call({call, Request, nil}, _From, {LastVnode}) ->
	Reply = spiny_erl_vnode:call(LastVnode, Request),
    {reply, Reply, {LastVnode}};

handle_call({call, Request, nil}, _From, State) ->
	Reply = {error, not_found},
    {reply, Reply, State};

handle_call({call, Request, Id}, _From, State) ->
    Reply = case ets:lookup(?TBL, Id) of
    	[{Id, Pid}] ->
    		spiny_erl_vnode:call(Pid, Request);
    	[] ->
    		{error, not_found}
    end,
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

