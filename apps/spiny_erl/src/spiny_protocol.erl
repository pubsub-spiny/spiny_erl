-module(spiny_protocol).

-export([start/0]).

start() ->
    %ok = application:start(cowboy),
    VRoutes = [{"/pub", spiny_http_pub, []}, {"/sub", spiny_http_sub, []}],
    Routes = [{'_',  VRoutes}],
    Dispatch = cowboy_router:compile(Routes),
    cowboy:start_http(http, 1024,
                    [{port, 8000}, {max_connections, infinity} |
                       [
                        {backlog, 1024}, {reuseaddr, true}, {linger, {false, 0}},
                        {send_timeout, 10000},
                        {reuse_sessions, false}, {verify, verify_none}
                      ]],
                      [{env, [{dispatch, Dispatch}]}]).
