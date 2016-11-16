-module(spiny_sub_util).


%% API
-export([match/2, add_topic/3, del_topic/3]).

match(Table, Topic) ->
    case ets:lookup(Table, Topic) of
        [] ->
            [];
        [{_, _, _, Nodes}] ->
            Nodes
    end.

add_topic(Table, Topic, Node) ->
    case ets:lookup(Table, Topic) of
        [] ->
            ets:insert(Table, {Topic, 1, maps:put(Node, 1, maps:new()), [Node]});
        [{_, TotalCnt, NodeMap, _}] ->
            NewNodeMap =
            case maps:find(Node, NodeMap) of
                error ->
                    maps:put(Node, 1, NodeMap);
                {ok, Cnt} ->
                    maps:put(Node, Cnt + 1, NodeMap)
            end,
            ets:insert(Table, {Topic, TotalCnt + 1, NewNodeMap,
                                        [N || {N, _} <- maps:to_list(NewNodeMap)]})
    end.

del_topic(Table, Topic, Node) ->
    case ets:lookup(Table, Topic) of
        [{_, TotalCnt, NodeMap, _}] ->
            {NewNodeMap, NewTotalCnt} =
            case maps:find(Node, NodeMap) of
                error ->
                    {NodeMap, TotalCnt};
                {ok, 1} ->
                    {maps:remove(Node, NodeMap), TotalCnt -1};
                {ok, Cnt} ->
                    {maps:put(Node, Cnt - 1, NodeMap), TotalCnt -1}
            end,
            case NewTotalCnt > 0 of
                true ->
                    ets:insert(Table, {Topic, NewTotalCnt, NewNodeMap,
                                                [N || {N, _} <- maps:to_list(NewNodeMap)]}),
                    ignore;
               false ->
                    ets:delete(Table, Topic)
            end;
        _ ->
            ignore
    end.