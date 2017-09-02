-module(sockerl_SUITE).
-export([init_per_suite/1
        ,end_per_suite/1
        ,init_per_testcase/2
        ,end_per_testcase/2
        ,all/0
        ,'1'/1
        ,'2'/1
        ,'3'/1
        ,'4'/1
        ,'5'/1
        ,'6'/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-define(HOST, "127.0.0.1").
-define(PORT, 8080).
-define(REUSEADDR_OPTS, [{reuseaddr, true}]).
-define(SOCK_OPTS(Opts), [{socket_options, lists:concat(Opts)}]).
-define(ACC_DBG_OPTS, [{acceptor_debug, [trace]}]).
-define(CON_DBG_OPTS, [{connector_debug, [trace]}]).
-define(SERVER_OPTS, ?ACC_DBG_OPTS ++ ?CON_DBG_OPTS ++ ?SOCK_OPTS([?REUSEADDR_OPTS])).
-define(B_SERVER_OPTS, ?ACC_DBG_OPTS ++ ?CON_DBG_OPTS ++ ?SOCK_OPTS([?REUSEADDR_OPTS, [{active, false}]])).
-define(B_CLIENT_OPTS, ?CON_DBG_OPTS ++ ?SOCK_OPTS([[{active, false}]])).
-define(KEYFILE_OPTS(PList), [{keyfile, get_key_file(PList)}]).
-define(CERTFILE_OPTS(PList), [{certfile, get_cert_file(PList)}]).
-define(SSL_TRANSPORTER_OPTS, [{transporter, sockerl_ssl_transporter}]).
-define(SSL_SERVER_OPTS(Config)
       ,?ACC_DBG_OPTS ++ ?CON_DBG_OPTS ++ ?SOCK_OPTS([?REUSEADDR_OPTS
                                                     ,?KEYFILE_OPTS(Config)
                                                     ,?CERTFILE_OPTS(Config)]) ++ ?SSL_TRANSPORTER_OPTS).
-define(SSL_CLIENT_OPTS, ?SSL_TRANSPORTER_OPTS ++ ?CON_DBG_OPTS).

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
    || Int <- lists:seq(1, erlang:length(?MODULE:module_info(exports)) - 8)].




'1'(_Config) ->
    TPid = erlang:self(),
    SRes = sockerl_server:start_link(testtt, fun() -> {ok, fun() -> {ok, [{state, TPid}]} end} end, ?PORT+1, ?SERVER_OPTS),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,
    ?assert(erlang:is_process_alive(SPid)),

    ?assertEqual([], sockerl:get_server_connections(SPid)),
    ?assertMatch([{1, _}], sockerl:get_acceptors(SPid)),

    CRes = sockerl_client:start_link(fun() -> {ok, [{state, TPid}]} end, ?HOST, ?PORT+1, ?CON_DBG_OPTS),
    ?assertMatch({ok, _}, CRes),
    {ok, CPid} = CRes,
    ?assert(erlang:is_pid(CPid) andalso erlang:is_process_alive(CPid)),

    SCons = sockerl:get_server_connections(SPid),
    ?assertMatch([{_, _}], SCons),
    [{SCRef, SCPid}] = SCons,
    ?assert(erlang:is_reference(SCRef)),
    ?assert(erlang:is_pid(SCPid)),
    ?assert(erlang:is_process_alive(SCPid)),

    erlang:spawn_link(fun() -> ?assertEqual(ok, sockerl:stop_connector(CPid)) end),
    handle_callback(sockerl_client, terminate, CPid, ok),

    handle_callback(sockerl_server, handle_disconnect, SCPid, ok),
    handle_callback(sockerl_server, terminate, SCPid, ok),
    timer:sleep(1),
    ?assertEqual([], sockerl:get_server_connections(SPid)),

    ?assertEqual(ok, sockerl:stop_server(SPid)),

    SRes2 = sockerl_server:start_link(fun() -> {ok, fun() -> {ok, [{state, TPid}]} end} end, ?PORT, ?SERVER_OPTS ++ [{acceptor_count, 2}]),
    ?assertMatch({ok, _Pid}, SRes2),
    {ok, SPid2} = SRes2,
    ?assert(erlang:is_process_alive(SPid2)),

    SAccs = sockerl:get_acceptors(SPid2),
    ?assertMatch([{_, _}, {_, _}], SAccs),

    [{_, SAccPid1}, {_, SAccPid2}] = SAccs,
    ?assert(erlang:is_process_alive(SAccPid1) andalso erlang:is_process_alive(SAccPid2)).
%%    ok.




'2'(_Config) ->
    AccCount = 10,
    TPid = erlang:self(),
    SRes = sockerl_server:start_link(fun() -> {ok, fun() -> {ok, [{state, TPid}]} end} end
                                    ,?PORT
                                    ,?SERVER_OPTS ++ [{acceptor_count, AccCount}]),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,
    ?assert(erlang:is_pid(SPid) andalso erlang:is_process_alive(SPid)),

    SAccs = sockerl:get_acceptors(SPid),
    ?assert(erlang:length(SAccs) == AccCount),

    ?assertEqual(accept, sockerl:get_acceptor_modes(SPid)),
    [?assertEqual(accept, sockerl_acceptor:get_mode(AccPid)) || {_Id, AccPid} <- SAccs],

    ?assertEqual(sleep, sockerl:change_acceptor_modes(SPid)),

    ?assertEqual(sleep, sockerl:get_acceptor_modes(SPid)),
    [?assertEqual(sleep, sockerl_acceptor:get_mode(AccPid)) || {_Id, AccPid} <- SAccs],

    ?assertEqual(ok, sockerl_acceptor:wakeup(erlang:element(2, erlang:hd(SAccs)))),
    ?assertEqual(not_allowed, sockerl:change_acceptor_modes(SPid)),

    ?assertEqual(ok, sockerl_acceptor:sleep(erlang:element(2, erlang:hd(SAccs)))),
    ?assertEqual(accept, sockerl:change_acceptor_modes(SPid)).
%%    ok.

'3'(Config) ->
    InitArg = fun() -> {ok, undefined} end,
    Res = sockerl_server:start_link(InitArg, ?PORT, ?SERVER_OPTS),
    ?assertMatch({ok, _Pid}, Res),
    {ok, SPid} = Res,
    ?assertEqual([], sockerl:get_server_connections(SPid)),
    Res2 = sockerl_server_sup:fetch_acceptors(SPid),
    ?assertMatch([{1, _Pid}], Res2),
    [{_, AccPid}] = Res2,
    ?assertEqual(accept, sockerl_server_sup:get_acceptor_modes(SPid)),
    ?assertEqual(accept, sockerl_server_sup:get_acceptor_modes(SPid)),
    ?assertEqual(accept, sockerl_acceptor:get_mode(AccPid)),
    ?assertEqual(ok, sockerl:sleep_acceptors(SPid)),
    ?assertEqual(sleep, sockerl_server_sup:get_acceptor_modes(SPid)),
    ?assertEqual(sleep, sockerl_acceptor:get_mode(AccPid)),
    ?assertEqual(ok, sockerl:wakeup_acceptors(SPid)),
    ?assertEqual(ok, sockerl:stop_server(SPid)),
    ?assertEqual(false, erlang:is_process_alive(SPid)),

    InitArg2 = fun() -> ignore end,
    ?assertEqual(ignore, sockerl_server:start_link(InitArg2, ?PORT+1, ?SERVER_OPTS)),

    InitArg3 = fun() -> {stop, oops} end,
    ?assertEqual({error, oops}, sockerl_server:start_link(InitArg3, ?PORT+2, ?SERVER_OPTS)),

    Val = "bad_return_value",
    InitArg4 = fun() -> Val end,
    ?assertMatch({error, {bad_return_value, [{returned_value, Val}|_]}}, sockerl_server:start_link(InitArg4, ?PORT+3, ?SERVER_OPTS)),
    Res3 = sockerl_server:start_link(InitArg, ?PORT, ?SSL_SERVER_OPTS(Config)),
    ?assertMatch({ok, _Pid}, Res3),
    sockerl_server_sup:stop(erlang:element(2, Res3)).


'4'(_Config) ->
    TPid = erlang:self(),
    SInitArg = fun() -> {ok, fun() -> {ok, [{state, TPid}]} end} end,
    SRes = sockerl_server:start_link(SInitArg, ?PORT+1, ?SERVER_OPTS),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,

    CInitArg = fun() -> {ok, [{state, TPid}
                             ,{packet, "1"}]} end,
    CRes = sockerl_client:start_link(CInitArg, ?HOST, ?PORT+1, ?CON_DBG_OPTS),
    ?assertMatch({ok, _Pid}, CRes),
    {ok, CConPid} = CRes,
    timer:sleep(1),
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
    receive
        {sockerl_client, handle_cast, Ref6, cast_req, _, _} ->
            CConPid ! {Ref6, ok}
    end,

    SConPid ! {'$gen_event', event},
    receive
        {sockerl_server, handle_event, Ref7, event, _, _} ->
            SConPid ! {Ref7, {ok, [{timeout, 100}]}}
    end,
    receive
        {sockerl_server, timeout, Ref8, _, SM} ->
            ?assertEqual(100, sockerl_metadata:get_timeout(SM)),
            SConPid ! {Ref8, {ok, [{packet, <<"5">>}]}}
    end,
    receive
        {sockerl_client, handle_packet, Ref9, "5", _, _} ->
            CConPid ! {Ref9, close}
    end,
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


'5'(Config) ->
    TPid = erlang:self(),
    SInitArg = fun() -> {ok, fun() -> {ok, [{state, TPid}]} end} end,
    SRes = sockerl_server:start_link(SInitArg
                                    ,?PORT
                                    ,?SSL_SERVER_OPTS(Config)),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,

    CInitArg = fun() -> {ok, [{state, TPid}
                             ,{packet, "1"}]} end,
    CRes = sockerl_client:start_link(CInitArg, ?HOST, ?PORT, ?SSL_CLIENT_OPTS),
    ?assertMatch({ok, _Pid}, CRes),
    {ok, CConPid} = CRes,

    timer:sleep(10),
    SCons = sockerl:get_server_connections(SPid),
    ?assertMatch([{_SConSock, _SConPid}], SCons),
    [{_, SConPid}] = SCons,
    %% don't use handle_callback, because we want to sure about ssl packet correctness.
    receive
        {sockerl_server, handle_packet, Ref, "1", _, _} ->
            SConPid ! {Ref, {ok, [{packet, "2"}]}}
    end,
    receive
        {sockerl_client, handle_packet, Ref2, "2", _, _} ->
            CConPid ! {Ref2, ok}
    end,
    sockerl_server_sup:stop(SPid).







'6'(_Config) ->
    TPid = erlang:self(),
    SInitArg = fun() -> {ok, fun() -> {ok, [{state, TPid}, {length, 1}]} end} end,
    SRes = sockerl_server:start_link(SInitArg, ?PORT, ?B_SERVER_OPTS),
    ?assertMatch({ok, _Pid}, SRes),
    {ok, SPid} = SRes,

    CInitArg = fun() -> {ok, [{state, TPid}
                             ,{packet, "12"}
                             ,{timeout, 0}]} end,
    CRes = sockerl_client:start_link(CInitArg, ?HOST, ?PORT, ?CON_DBG_OPTS),%?B_CLIENT_OPTS),
    ?assertMatch({ok, _Pid}, CRes),
    {ok, CConPid} = CRes,
    timer:sleep(1),
    SCons = sockerl:get_server_connections(SPid),
    ?assertMatch([{_SConSock, _SConPid}], SCons),
    [{_, SConPid}] = SCons,

    handle_callback(sockerl_server, handle_packet, SConPid, {ok, [{packet, ""}, {timeout, 0}, {length, 0}]}),

    handle_callback(sockerl_client, timeout, CConPid, {ok, [{srtimeout, 100}, {timeout, infinity}]}),

    handle_callback(sockerl_server, timeout, SConPid, {ok, [{timeout, 0}]}),

    handle_callback(sockerl_server, handle_packet, SConPid, {ok, [{packet, <<"12345">>}]}),

    handle_callback(sockerl_client, handle_packet, CConPid, close),

    handle_callback(sockerl_client, terminate, CConPid, ok),

    handle_callback(sockerl_server, timeout, SConPid, {ok, [{timeout, infinity}]}),

    handle_callback(sockerl_server, handle_disconnect, SConPid, ok),

    handle_callback(sockerl_server, terminate, SConPid, ok).







handle_callback(Mod, Func, Pid, Reply) ->
    receive
        {Mod, Func, Ref, State, SMD} ->
            Pid ! {Ref, Reply},
            {State, SMD};
        {Mod, Func, Ref, Arg, State, SMD} ->
            Pid ! {Ref, Reply},
            {Arg, State, SMD};
        {Mod, Func, Ref, Arg1, Arg2, State, SMD} ->
            Pid ! {Ref, Reply},
            {Arg1, Arg2, State, SMD}
    after 5000 ->
        ct:pal("Did not receive callback message for ~p:~p and could not send ~p to ~p~n", [Mod, Func, Reply, Pid]),
        erlang:exit({timeout, {Mod, Func, Pid, Reply}})
    end.







get_key_file(Config) ->
    Dir = ?config(data_dir, Config),
    filename:join(Dir, "key.pem").


get_cert_file(Config) ->
    Dir = ?config(data_dir, Config),
    filename:join(Dir, "cert.pem").
