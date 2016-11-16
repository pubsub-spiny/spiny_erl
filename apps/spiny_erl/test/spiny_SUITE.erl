-module(spiny_SUITE).

-compile(export_all).

all() ->
	[cluster_test].

init_per_suite(_Config) ->
    %os:cmd(os:find_executable("epmd")++" -daemon"),
    {ok, Hostname} = inet:gethostname(),
    case net_kernel:start([list_to_atom("runner@"++Hostname), shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    %lager:info("node name ~p", [node()]),
    _Config.

end_per_suite(_Config) ->
    application:stop(lager),
    _Config.

cluster_test(_Config) ->
    A = start_node(cluster_test_a),
    B = start_node(cluster_test_b),
    wait_running(A),
    wait_running(B),
    %true = spiny_erl_app:is_running(Z),
    Node = node(),
    R = rpc:call(B, spiny_erl_app, join, [A]),
    ct:log("A State:~p~n", [rpc:call(A, spiny_erl_app, state, [])]),
    ct:log("B State:~p~n", [rpc:call(B, spiny_erl_app, state, [])]),
    %ct:log("A stabilize:~p~n", [rpc:call(A, spiny_erl_vnode, call, [stabilize])]),
    %ct:log("B stabilize:~p~n", [rpc:call(B, spiny_erl_vnode, call, [stabilize])]),
    timer:sleep(7000),
    ct:log("A State:~p~n", [rpc:call(A, spiny_erl_app, state, [])]),
    ct:log("B State:~p~n", [rpc:call(B, spiny_erl_app, state, [])]),
    RPA = rpc:call(A, spiny_erl_app, publish, ["topic", <<"payload">>]),
    RPB = rpc:call(B, spiny_erl_app, publish, ["topic", <<"payload">>]),
    %_ = rpc:call(B, spiny_erl_app, subscribe, ["topic"]),
    ct:log("Result:~p,~p,~p~n", [R, RPA, RPB]),
    ct_slave:stop(A).



start_node(Name) ->
    CodePath = lists:filter(fun filelib:is_dir/1, code:get_path()),
    %% have the slave nodes monitor the runner node, so they can't outlive it
    NodeConfig = [
            {monitor_master, true},
            {startup_functions, [
                    {code, set_path, [CodePath]}
                    ]}],
    case ct_slave:start(Name, NodeConfig) of
        {ok, Node} ->
            ok = rpc:call(Node, application, load, [cowboy]),
		    rpc:call(Node, application, ensure_all_started, [spiny_erl]),
		    Node;
		{error, already_started, Node} ->
            ct_slave:stop(Name),
            start_node(Name);
		{error, Reason, Node} ->
			io:format("error ~p~n", [Reason])
	end.

wait_running(Node) ->
    wait_running(Node, 30000).

wait_running(Node, Timeout) when Timeout < 0 ->
    throw({wait_timeout, Node});

wait_running(Node, Timeout) ->
    case rpc:call(Node, spiny_erl_app, is_running, []) of
        true  -> ok;
        false -> timer:sleep(100),
                 wait_running(Node, Timeout - 100)
    end.