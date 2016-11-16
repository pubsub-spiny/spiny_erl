%%%-------------------------------------------------------------------
%% @doc spiny_erl_rendezvous public API
%% rendezvouz based routing
%% sn(key), en(key)
%% @end
%%%-------------------------------------------------------------------

-module(spiny_erl_rendezvous).

%% API
-export([sn/1, en/1]).



%%====================================================================
%% API
%%====================================================================

sn(Key) when is_binary(Key) ->
	sn(binary_to_list(Key));
sn(Key) when is_list(Key) ->
	spiny_erl_vnode:lookup(chord_lib:hash(Key)).


en(Key) when is_binary(Key) ->
	sn(binary_to_list(Key));
en(Key) when is_list(Key) ->
	spiny_erl_vnode:lookup(chord_lib:hash(Key)).


