-module(sockerl_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-define(HOST, "127.0.0.1").
-define(PORT, 8080).
-define(REUSE_ADDR, {reuseaddr, true}).
-define(REUSE_ADDR_START_OPTIONS, [{socket_options, [?REUSE_ADDR]}]).
-define(ACC_DBG, {acceptor_debug, [trace]}).
-define(CON_DBG, {connector_debug, [trace]}).

init_per_suite(Config) ->
    application:start(sasl),
    Config.



end_per_suite(_Config) ->
    application:stop(sasl),
    ok.



init_per_testcase(_TCName, Config) ->
    erlang:process_flag(trap_exit, true),
    Config.



end_per_testcase(_TCName, _Config) ->
    ok.





all() ->
    [erlang:list_to_atom(erlang:integer_to_list(Int))
    || Int <- lists:seq(1, 4)].



'1'(Config) ->
    InitArg = fun() -> {ok, undefined} end,
    Res = sockerl_server:start_link(InitArg, ?PORT, ?REUSE_ADDR_START_OPTIONS),
    ?assertMatch({ok, _Pid}, Res),
    {ok, SPid} = Res,
    ?assertEqual([], sockerl:get_server_connections(SPid)),
    Res2 = sockerl_server_sup:fetch_acceptors(SPid),
    ?assertMatch([{1, _Pid}], Res2),
    [{_, AccPid}] = Res2,
    accept = sockerl_server_sup:get_acceptor_modes(SPid),
    ?assertEqual(accept, sockerl_acceptor:get_mode(AccPid)),
    ?assertEqual(ok, sockerl:sleep_acceptors(SPid)),
    sleep = sockerl_server_sup:get_acceptor_modes(SPid),
    ?assertEqual(sleep, sockerl_acceptor:get_mode(AccPid)),
    ?assertEqual(ok, sockerl:wakeup_acceptors(SPid)),
    ?assertEqual(ok, sockerl:stop_server(SPid)),
    ?assertEqual(false, erlang:is_process_alive(SPid)),

    InitArg2 = fun() -> ignore end,
    ?assertEqual(ignore, sockerl_server:start_link(InitArg2, ?PORT+1, ?REUSE_ADDR_START_OPTIONS)),

    InitArg3 = fun() -> {stop, oops} end,
    ?assertEqual({error, oops}, sockerl_server:start_link(InitArg3, ?PORT+2, ?REUSE_ADDR_START_OPTIONS)),

    Val = "bad_return_value",
    InitArg4 = fun() -> Val end,
    ?assertMatch({error, {bad_return_value, [{returned_value, Val}|_]}}, sockerl_server:start_link(InitArg4, ?PORT+3, ?REUSE_ADDR_START_OPTIONS)),
    Key = get_key_file(Config),
    Cert = get_cert_file(Config),
    Res3 = sockerl_server:start_link(InitArg, ?PORT, [{transporter, sockerl_ssl_transporter}
                                                     ,{socket_options, [{keyfile, Key}
                                                                       ,{certfile, Cert}
                                                                       ,?REUSE_ADDR]}]),
    ?assertMatch({ok, _Pid}, Res3),
    sockerl_server_sup:stop(erlang:element(2, Res3)).





'2'(_Config) ->
    TPid = erlang:self(),
    SInitArg = fun() -> {ok, fun() -> {ok, [{state, TPid}]} end} end,
    SRes = sockerl_server:start_link(SInitArg, ?PORT+1, [?ACC_DBG, ?CON_DBG, {socket_options, [?REUSE_ADDR]}]),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,

    CInitArg = fun() -> {ok, [{state, TPid}
                             ,{packet, "1"}]} end,
    CRes = sockerl_client:start_link(CInitArg, ?HOST, ?PORT+1, [?CON_DBG]),
    ?assertMatch({ok, _Pid}, CRes),
    {ok, CConPid} = CRes,
    SCons = sockerl:get_server_connections(SPid),
    ?assertMatch([{_SConSock, _SConPid}], SCons),
    [{_, SConPid}] = SCons,
    receive
        {sockerl_server, handle_packet, Ref, "1", _, _} ->
            SConPid ! {Ref, {ok, [{packet, "2"}]}}
    end,
    receive
        {sockerl_client, handle_packet, Ref2, "2", _, _} ->
            CConPid ! {Ref2, ok}
    end,

    sockerl:send_async(SConPid, <<"3">>),
    receive
        {sockerl_client, handle_packet, Ref3, "3", _, _} ->
            CConPid ! {Ref3, ok}
    end,

    sockerl:send_sync(CConPid, <<"4">>),
    receive
        {sockerl_server, handle_packet, Ref4, "4", _, _} ->
            SConPid ! {Ref4, ok}
    end,

    CallerFun =
        fun() ->
            Res = gen_server:call(SConPid, call_req),
            TPid ! {erlang:self(), Res},
            erlang:exit(normal)
        end,
    CallerPid = erlang:spawn(CallerFun),
    io:format("1~n"),

    receive
        {sockerl_server, handle_call, Ref5, call_req, {CallerPid, _}=Tag, _, _} ->
            SConPid ! {Ref5, {ok, [{reply, Tag, done}]}},
            receive
                {CallerPid, done} ->
                    pass
            end
    end,

    gen_server:cast(CConPid, cast_req),
    io:format("2~n"),
    receive
        {sockerl_client, handle_cast, Ref6, cast_req, _, _} ->
            CConPid ! {Ref6, ok}
    end,

    SConPid ! {'$gen_event', event},
    io:format("3~n"),
    receive
        {sockerl_server, handle_event, Ref7, event, _, _} ->
            SConPid ! {Ref7, {ok, [{timeout, 100}]}}
    end,
    io:format("4~n"),
    receive
        {sockerl_server, timeout, Ref8, _, SM} ->
            ?assertEqual(100, sockerl_metadata:get_timeout(SM)),
            SConPid ! {Ref8, {ok, [{packet, <<"5">>}]}}
    end,
    io:format("5~n"),
    receive
        {sockerl_client, handle_packet, Ref9, "5", _, _} ->
            CConPid ! {Ref9, close}
    end,
    io:format("6~n"),
    receive
        {sockerl_client, terminate, Ref10, normal, _, _} ->
            CConPid ! {Ref10, ok}
    end,

    receive
        {sockerl_server, handle_disconnect, Ref11, _, _} ->
            SConPid ! {Ref11, ok}
    end,
    receive
        {sockerl_server, terminate, Ref12, normal, _, _} ->
            SConPid ! {Ref12, ok}
    end.







'3'(Config) ->
    TPid = erlang:self(),
    SInitArg = fun() -> {ok, fun() -> {ok, [{state, TPid}]} end} end,
    Key = get_key_file(Config),
    Cert = get_cert_file(Config),
    SRes = sockerl_server:start_link(SInitArg
                                    ,?PORT
                                    ,[?ACC_DBG
                                     ,?CON_DBG
                                     ,{transporter, sockerl_ssl_transporter}
                                     ,{socket_options
                                      ,[{keyfile, Key}
                                       ,{certfile, Cert}
                                       ,?REUSE_ADDR]}]),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,

    CInitArg = fun() -> {ok, [{state, TPid}
                             ,{packet, "1"}]} end,
    CRes = sockerl_client:start_link(CInitArg, ?HOST, ?PORT, [{transporter, sockerl_ssl_transporter}, ?CON_DBG]),
    ?assertMatch({ok, _Pid}, CRes),
    {ok, CConPid} = CRes,

    SCons = sockerl:get_server_connections(SPid),
    ?assertMatch([{_SConSock, _SConPid}], SCons),
    [{_, SConPid}] = SCons,
    receive
        {sockerl_server, handle_packet, Ref, "1", _, _} ->
            SConPid ! {Ref, {ok, [{packet, "2"}]}}
    end,
    receive
        {sockerl_client, handle_packet, Ref2, "2", _, _} ->
            CConPid ! {Ref2, ok}
    end,
    sockerl_server_sup:stop(SPid).







'4'(_Config) ->
    TPid = erlang:self(),
    SInitArg = fun() -> {ok, fun() -> {ok, [{state, TPid}, {length, 1}]} end} end,
    SRes = sockerl_server:start_link(SInitArg, ?PORT, [?ACC_DBG, ?CON_DBG, {socket_options, [{active, false}, ?REUSE_ADDR]}]),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,

    CInitArg = fun() -> {ok, [{state, TPid}
                             ,{packet, "12"}
                             ,{timeout, 0}]} end,
    CRes = sockerl_client:start_link(CInitArg, ?HOST, ?PORT, [?CON_DBG, {active, false}]),
    ?assertMatch({ok, _Pid}, CRes),
    {ok, CConPid} = CRes,
    timer:sleep(1),
    SCons = sockerl:get_server_connections(SPid),
    ?assertMatch([{_SConSock, _SConPid}], SCons),
    [{_, SConPid}] = SCons,
    io:format("1~n"),
%%    receive
%%        Msg ->
%%            io:format("~p~n", [Msg])
%%    end,
%%    receive
%%        Msg2 ->
%%            io:format("~p~n", [Msg2])
%%    end,
    receive
        {sockerl_server, handle_packet, Ref, "1", _, _} ->
            SConPid ! {Ref, {ok, [{packet, ""}, {timeout, 0}, {length, 0}]}}
    end,
    io:format("2~n"),
    receive
        {sockerl_client, timeout, Ref2, _, _} ->
            CConPid ! {Ref2, {ok, [{srtimeout, 100}, {timeout, infinity}]}}
    end,
    io:format("3~n"),
    receive
        {sockerl_server, timeout, Ref3, _, _} ->
            SConPid ! {Ref3, {ok, [{timeout, 0}]}}
    end,
    io:format("4~n"),
    receive
        {sockerl_server, handle_packet, Ref4, "2", _, _} ->
            SConPid ! {Ref4, {ok, [{packet, <<"12345">>}]}}
    end,
    io:format("5~n"),
    receive
        {sockerl_client, handle_packet, Ref5, "12345", _, _} ->
            CConPid ! {Ref5, close}
    end,
    io:format("6~n"),
    receive
        {sockerl_client, terminate, Ref6, normal, _, _} ->
            CConPid ! {Ref6, ok}
    end,
    io:format("7~n"),
    receive
        {sockerl_server, timeout, Ref7, _, _} ->
            SConPid ! {Ref7, {ok, [{timeout, infinity}]}}
    end,
    io:format("8~n"),
    receive
        {sockerl_server, handle_disconnect, Ref8, _, _} ->
            SConPid ! {Ref8, ok}
    end,
    receive
        {sockerl_server, terminate, Ref9, normal, _, _} ->
            SConPid ! {Ref9, ok}
    end.

















get_key_file(Config) ->
    Dir = ?config(data_dir, Config),
    filename:join(Dir, "key.pem").


get_cert_file(Config) ->
    Dir = ?config(data_dir, Config),
    filename:join(Dir, "cert.pem").
