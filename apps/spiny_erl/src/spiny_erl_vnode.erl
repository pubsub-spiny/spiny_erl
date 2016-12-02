%%%-------------------------------------------------------------------
%% @doc spiny_erl_vnode public API
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_vnode).

-behaviour(gen_server).

-include("spiny_erl.hrl").

%% API
-export([start_link/2, call/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% TODO: Change successor to list of successors


-record(state, {
    id,
    key,
    predecessor=nil,
    finger_table=[],
    successor=#vnode{}}).

-define('MAX_KEY', chord_lib:max_hash_value()).

%%====================================================================
%% API
%%====================================================================




%%--------------------------------------------------------------------
%% @doc
%% Starts the node and attempts to place itself in the correct
%% location on the ring according to the known nodes.
%%
%% @spec start_link(Id) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Id, KnownVnode) ->
    io:format("spiny_erl_vnode:start_link:~p~n", [Id]),
    gen_server:start_link(?MODULE, {Id, KnownVnode}, []).
    
    
%%--------------------------------------------------------------------
%% @doc
%% Shortcut for gen_server:call({gen_chord, node()}, Request)
%%
%% @spec call(Request::term(), node()) -> Reply::term()
%% @end
%%--------------------------------------------------------------------
%% call to vnode
call(Pid, Request) ->
    io:format("spiny_erl_vnode:Calling ~p with message:~p~n", [Pid, Request]),
    gen_server:call(Pid, Request).

call_vnode(Request, VNode) ->
	io:format("spiny_erl_vnode:Calling vnode ~p with message:~p~n", [VNode, Request]),
    spiny_erl_vnode_man:call_vnode(Request, VNode).
    


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Id, nil}) ->
    io:format("Starting vnode...~p~n", [Id]),
    % generate virtual node id
    Key = chord_lib:hash(Id),
    spawn_link(fun() -> periodic_stabilize() end),
    {ok, #state{
        id=Id,
        key=Key,
        successor=#vnode{id=Id, node=node()}
    }};
init({Id, KnownVnode}) ->
    io:format("Starting vnode with seed node...~p, ~p~n", [Id, KnownVnode]),
    % generate virtual node id
    Key = chord_lib:hash(Id),
    spawn_link(fun() -> periodic_stabilize() end),
    {ok, Successor} = find_successor(Key, KnownVnode),
    SelfVnode = #vnode{id=Id, node=node()},
    notify_successor(Successor, SelfVnode),
    {ok, #state{
        id=Id,
        key=Key,
        successor=Successor
    }}.

self_vnode(State) ->
    #vnode{id=State#state.id, node=node()}.

handle_call({join, Node}, _From, #state{key= Key} = State) ->
    {ok, Successor} = find_successor(Key, #vnode{id=nil, node=Node}),
    Reply = notify_successor(Successor, self_vnode(State)),
    {reply, Reply, State#state{successor= Successor}};

handle_call({lookup, Key}, _From, State) ->
    handle_call({find_successor, Key}, _From, State);

handle_call({find_successor, Key}, _From, State) ->
    io:format("Finding successor to ~p...~n", [Key]),
    CurrentSuccessor = State#state.successor,
    Successor = case successor_is_successor(Key, State) of
        true -> CurrentSuccessor;
        % TODO: Substitute this finde_successor call with a closest_preceeding_node
        % call to abstract out the finger table lookup
        false -> 
            {ok, Node} = find_successor(Key, CurrentSuccessor),
            Node
    end,
    Reply = {ok, Successor},
    {reply, Reply, State};

handle_call({new_predecessor, NewPredecessor}, From, State) ->
    NewPredecessorKey = chord_lib:hash(NewPredecessor#vnode.id),
    OldPredecessor = State#state.predecessor,
    OldPredecessorId = case OldPredecessor of
        nil -> nil;
        OldPredecessorVnode -> OldPredecessorVnode#vnode.id
    end,
    OldPredecessorKey = chord_lib:hash(OldPredecessorId),
    SuccessorKey = State#state.key,
    IsPredecessor = (OldPredecessor =:= nil) orelse between(OldPredecessorKey, SuccessorKey, NewPredecessorKey, false),
    NewState = case IsPredecessor of
        true -> 
            %error_logger:info_msg("New predecessor: ~p~n", [NewPredecessor]), 
            State#state{predecessor=NewPredecessor};
        false -> State
    end,
    Successor = State#state.successor,
    SuccessorIsSelf = State#state.id =:= Successor#vnode.id,
    NewState2 = case SuccessorIsSelf of
        true -> 
            gen_server:reply(From, ok),
            %error_logger:info_msg("New successor: ~p~n", [NewPredecessor]),
            notify_successor(NewPredecessor, self_vnode(State)),
            NewState#state{successor=NewPredecessor};
        false -> NewState
    end,
    Reply = ok,
    {reply, Reply, NewState2};


handle_call(predecessor, _From, State) ->
    Reply = {ok, State#state.predecessor},
    {reply, Reply, State};
handle_call(state, _From, State) ->
    {reply, State, State};
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

handle_info(stabilize, State) ->
    io:format("Stabilizing... State: ~p~n", [State]),
    erlang:send_after(5000+round(random:uniform() * 1000), self(), stabilize),
    Successor = State#state.successor,
    SuccessorKey = chord_lib:hash(Successor),
    Predecessor = State#state.predecessor,
    PredecessorKey = chord_lib:hash(Predecessor),
    {ok, SuccessorPredecessor} = case Successor =:= node() of
        true -> {ok, node()};
        _ -> call_vnode(predecessor, Successor)
    end,
    io:format("Got SuccessorPredecessor: ~p~n", [SuccessorPredecessor]),
    SuccessorPredecessorKey = chord_lib:hash(SuccessorPredecessor),
    NewState = case (SuccessorPredecessor /= node()) andalso between(PredecessorKey, SuccessorKey, SuccessorPredecessorKey, false) of
        true -> 
            io:format("New successor: ~p~n", SuccessorPredecessor),
            notify_successor(SuccessorPredecessor, self_vnode(State)),
            State#state{successor=SuccessorPredecessor};
        false -> State
    end,
    io:format("Stabilized state: ~p~n", [NewState]),
    {noreply, NewState};
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

notify_successor(#vnode{id=Id, node=SuccessorNode}=Successor, SelfVnode) ->
    Node = node(),
    case SuccessorNode of
        nil -> ok;
        Node -> ok;
        _ -> call_vnode({new_predecessor, SelfVnode}, Successor)
    end.

successor_is_successor(Key, State) ->
    PredecessorKey = State#state.key,
    Successor = State#state.successor,
    SuccessorId = Successor#vnode.id,
    SuccessorKey = chord_lib:hash(SuccessorId),
    IsSuccessor = (SuccessorId =:= State#state.id) and (Successor#vnode.node =:= node()),
    io:format("successor_is_successor1 ~p,~p~n", [SuccessorId, State#state.id]),
    IsBetween = between(PredecessorKey, SuccessorKey, Key, true),
    io:format("successor_is_successor2 ~p,~p~n", [IsSuccessor, IsBetween]),
    IsSuccessor or IsBetween.
    
between(PredecessorKey, SuccessorKey, Key, IsInclusive) ->
    io:format("between ~p,~p,~p,~p~n", [PredecessorKey, SuccessorKey, Key, IsInclusive]),
    IsLastNode = SuccessorKey =< PredecessorKey,
    Attempt1 = case IsInclusive of
        true -> (PredecessorKey < Key) and (Key =< SuccessorKey);
        false -> (PredecessorKey < Key) and (Key < SuccessorKey)
    end,
    Attempt2 = case IsInclusive of
        true -> IsLastNode and (((PredecessorKey < Key) and (Key =< ?MAX_KEY)) or 
                                ((Key >= 0) and (Key =< SuccessorKey)));
        false -> IsLastNode and (((PredecessorKey < Key) and (Key =< ?MAX_KEY)) or 
                                 ((Key >= 0) and (Key < SuccessorKey)))
    end,
    IsBetween = Attempt1 or Attempt2,
    %error_logger:info_msg("Checking if ~p is between ~p and ~p...~p~n", [Key, PredecessorKey, SuccessorKey, IsBetween]),
    IsBetween.
    
find_successor(Key, VNode) ->
    io:format("spiny_erl_vnode:find_successor(~p, ~p)~n", [Key, VNode]),
    IsInRange = Key >= 0 andalso Key =< ?MAX_KEY,
    case IsInRange of
        true -> spiny_erl_vnode_man:call_vnode({find_successor, Key}, VNode);
        false -> {error, out_of_bounds}
    end.

periodic_stabilize() ->
    erlang:send_after(1, self(), stabilize).
