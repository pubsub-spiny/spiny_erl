-module(spiny_http_pub).

-behavior(cowboy_handler).

-export([init/3,
         handle/2,
         terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined}.


handle(Req, State) ->
	{Path, Req2} = cowboy_req:path(Req),
	{Channel, Req3} = cowboy_req:qs_val(<<"c">>, Req2),
	{Value, Req4} = cowboy_req:qs_val(<<"v">>, Req3),
	Res = spiny_erl_app:publish(Channel, Value),
    {ContentType, Req5} = cowboy_req:header(<<"content-type">>, Req4,
                                            <<"text/plain">>),
    {ok, reply(Req5, ContentType, Res), State}.

terminate(_Reason, _Req, _State) ->
    ok.

reply(Req, <<"text/plain">>, Res) ->
    Output = <<"body">>,
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
                                  Output, Req),
    Req2.