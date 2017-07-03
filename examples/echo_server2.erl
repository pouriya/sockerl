-module(echo_server2).
-behaviour(sockerl).

%% API:
-export([start/1
        ,packet_count/1]).


%% sockerl callbacks:
%% I can implement handle_cast/3, handle_event/3, timeout/2 too.
%% But i don't need them yet.
-export([listen_init/2
        ,connector_init/2
        ,handle_packet/3
        ,handle_disconnect/2
        ,handle_call/4
        ,terminate/3
        ,code_change/3]).





start(Port) ->
    RegisterName = {local, ?MODULE}, %% Optional
    CallbackMod = ?MODULE,
    InitArg = null,
    StartOpts = [],
    sockerl:start_link_server(RegisterName
                             ,CallbackMod
                             ,InitArg
                             ,Port
                             ,StartOpts).





packet_count(Con) ->
    gen_server:call(Con, how_much).





-spec
listen_init(InitArg, ListenSock) ->
    'ok'             |
    {'ok', InitArg2} |
    {'stop', Reason} |
    'ignore'
when
    InitArg :: any(),
    ListenSock :: sockerl_types:socket(),
    InitArg2 :: any(),
    Reason :: any().
%% This function will be called after binding socket to given port.
%% Note that InitArg or InitArg2 will be used as InitArg of 
%%  connection_init/2 for every incoming connection.
listen_init(null, _LSock) ->
    {ok, null2}.





-spec
connector_init(InitArg, ConnectionSock) ->
    'ok'                    | %% your State will be atom 'undefined'
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}  |
    'ignore'
when
    InitArg :: any(),
    ConnectionSock :: sockerl_types:socket(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'packet', sockerl_types:packet()},
    Reason :: any().
%% Every accepted connection has its own process and state and after 
%%  accepting this function will be called by its process.
%% If returns 'close' or {'close', Opts}, process will close socket 
%%  connection and will exit with reason 'normal'
%% If returns {'stop', ...} , by default entire server will crash. But 
%%  you can control behaviour of 'stop' too.
%% If you use {packet, Pkt}, it will send Pkt in initializing position.
%% I will explain other available options for Opts later.
connector_init(null2, _ConSock) ->
    {ok, [{state, 0}]}.





-spec
handle_packet(Packet, State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}
when
    Packet :: sockerl_types:packet(),
    State :: any(),
    Metadata :: sockerl_types:metadata(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'packet', sockerl_types:packet()},
    Reason :: any().
%% For every incoming packet from socket, process will execute this 
%%  function with that packet.
%% Don't think about MetaData, i will explain that later.
%% This is echo server which means server will send back every packet.
handle_packet(Pkt, Count = _MyState, _MetaData) ->
    {ok, [{state, Count + 1}, {packet, Pkt}]}.





-spec
handle_call(Request, From, State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}
when
    Request :: any(),
    From :: {pid(), reference()},
    State :: any(),
    Metadata :: sockerl_types:metadata(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'reply', From, Response}
         | {'packet', sockerl_types:packet()},
    Response :: any(),
    Reason :: any().
%% Note that i do'nt want to change state (Count) in my call, then i
%% don't need to return state again.
%% I want to send to caller the number of client packets that it sent.
handle_call(how_much, From, Count, _Metadata) ->
    {ok, [{reply, From, Count}]}.





-spec
handle_disconnect(State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}
when
    State :: any(),
    Metadata :: sockerl_types:metadata(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()},
    Reason :: any().
%% This function will be called when socket connection closed by remote.
%% Don't use {'packet', Pkt} in return value because socket has been
%%  closed and after this terminate/3 will called.
%% Metadata is an Erlang record containing some data, one of them is
%%  socket.
%% You can use API of sockerl_metadata module for working on Metadata.
handle_disconnect(Count, Metadata) ->
    Sock = sockerl_metadata:get_socket(Metadata),
    io:format("Socket ~p closed connection after sending ~p packets~n"
             ,[Sock, Count]),
    close.





-spec
terminate(Reason, State, Metadata) ->
    any()
    when
Reason :: any(),
    State :: any(),
    Metadata :: sockerl_types:metadata().
%% Why i should implement handle_disconnect and terminate both?
%% Because you can change reason termination in handle_disconnect when 
%%  remote closed connection.
terminate(_Reason, _State, _Metadata) ->
    ok.





-spec
code_change(OldVsn, State, Extra) ->
    {'ok', NewState}
    when
    OldVsn :: any(),
    State :: any(),
    Extra :: any(),
    NewState :: any().
code_change(_, State, _) ->
    {ok, State}.
