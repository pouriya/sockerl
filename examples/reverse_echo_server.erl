-module(reverse_echo_server).
-behaviour(sockerl).

%% API:
-export([start/1
        ,print_buffer/1
        ,clean/1]).


%% sockerl callbacks:
-export([listen_init/2
        ,connector_init/2
        ,handle_packet/3
        ,handle_disconnect/2
        ,handle_call/4
        ,handle_cast/3
        ,terminate/3
        ,code_change/3
        ,timeout/2
        ,srtimeout/2]).





start(Port) ->
    RegisterName = {local, ?MODULE}, %% Optional
    CallbackMod = ?MODULE,
    InitArg = null,

    StartOpts = [{acceptor_count, 3}
                ,{acceptor_mode, sleep}
                ,{acceptor_debug, [trace]}

                ,{connector_debug, [trace]}

                ,{socket_options, [{active, false}, binary]}],

    sockerl:start_link_server(RegisterName
                             ,CallbackMod
                             ,InitArg
                             ,Port
                             ,StartOpts).





print_buffer(Con) ->
    gen_server:call(Con, print_buffer).


clean(Con) ->
    gen_server:cast(Con, clean).





listen_init(null, _LSock) ->
    ok.




%% In passive mode: 
%% When process is waiting for socket packet, it can't receive Erlang messages.
%% When process is waiting for Erlang message, it can't receive socket packets.
%% So i need to define timeout for Erlang receive and socket receive.
%% Use {'timeout', timeout()} for Erlang receive, after timeout if
%%  process did not get message, callback 'timeout/2' will be called.
%% Use {'srtimeout', timeout()} (SRTimeout: Socket Receive Timeout) for
%%  socket receive, after timeout if process did not get packet,
%%  callback 'srtimeout/2' will be called.
%% In passive mode if you want to read for example 4 bytes from socket,
%%  you can define {'length', positive_integer()} in return of every
%%  callback.
%% If you want to use your previous length, timeout or srtimeout value
%%  for increasing or decrasing, etc, dont put it in your state,
%%  Metadata has them.
%% #sockerl_metadata{socket = Sock
%%                  ,timeout = Timeout
%%                  ,srtimeout = SRTimeout
%%                  ,length = Len
%%                  ,transporter = TrMod
%%                  ,options = Opts}
%% Just use sockerl_metadata API.
connector_init(null, _ConSock) ->
    {ok, [{state, <<"">>}, {timeout, 1}, {srtimeout, 1}]}.





handle_packet(Pkt, Buff, _MetaData) ->
    {ok, [{packet, bin_rev(Pkt)}, {state, <<Buff/binary, Pkt/binary>>}]}.





handle_call(print_buffer, From, Buff, _Metadata) ->
    io:format("Buffer: ~p~n", [Buff]),
    {ok, [{reply, From, ok}]}.




-spec
handle_cast(Cast, State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}
when
    Cast :: any(),
    State :: any(),
    Metadata ::  sockerl_types:metadata(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'packet', sockerl_types:packet()}
         | {'timeout', timeout()}
         | {'srtimeout', timeout()}
         | {'length', sockerl_types:length()},
    Reason :: any().
handle_cast(clean, _Buff, _Metadata) ->
    {ok, [{state, <<"">>}]}.





handle_disconnect(_Buff, _Metadata) ->
    ok.





terminate(_Reason, _State, _Metadata) ->
    ok.





code_change(_, State, _) ->
    {ok, State}.




-spec
timeout(State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}
when
    State :: any(),
    Metadata ::  sockerl_types:metadata(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'packet', sockerl_types:packet()}
         | {'timeout', timeout()}
         | {'srtimeout', timeout()}
         | {'length', sockerl_types:length()},
    Reason :: any().
timeout(_Buff, _Metadata) ->
    ok.




-spec
srtimeout(State, Metadata) ->
    'ok'                    |
    {'ok', Opts}            |
    'close'                 |
    {'close', Opts}         |
    {'stop', Reason}        |
    {'stop', Reason, Opts}
when
    State :: any(),
    Metadata ::  sockerl_types:metadata(),
    Opts :: [] | [Opt],
    Opt :: {'state', any()}
         | {'packet', sockerl_types:packet()}
         | {'timeout', timeout()}
         | {'srtimeout', timeout()}
         | {'length', sockerl_types:length()},
    Reason :: any().
srtimeout(_Buff, _Metadata) ->
    ok.





%% Reverses binary
bin_rev(Bin) ->
    Size = erlang:size(Bin)*8,
    <<X:Size/integer-little>> = Bin,
    <<X:Size/integer-big>>.
