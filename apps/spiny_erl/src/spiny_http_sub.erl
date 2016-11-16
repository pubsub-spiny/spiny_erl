-module(spiny_http_sub).

-behavior(cowboy_loop_handler).

-export([init/3,
         info/3,
         terminate/3]).

init(_Type, Req, _Opts) ->
	{Path, Req2} = cowboy_req:path(Req),
	{Channel, Req3} = cowboy_req:qs_val(<<"c">>, Req2),
	spiny_erl_app:subscribe(Channel, self()),
	io:format("init~n"),
    {loop, Req3, {Channel}, hibernate}.

handle(Info, Req, State) ->
	io:format("handle~n"),
	{ok, Req, State}.

info(Info, Req, State) ->
	io:format("info~n"),
    {ContentType, Req2} = cowboy_req:header(<<"content-type">>, Req,
                                            <<"application/json">>),
    {ok, reply(Req2, ContentType, Info), State}.

terminate(_Reason, _Req, {Channel}) ->
	io:format("terminate~n"),
	spiny_erl_app:unsubscribe(Channel, self()),
    ok.

reply(Req, <<"application/json">>, {Topic, Msg}) ->
    Output = Msg,
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
                                  Output, Req),
    Req2.