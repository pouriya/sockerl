-module(pool_check_echo_server2).
-behaviour(sockerl).

-export([start_link/2
        ,connector_init/2
        ,handle_packet/3
        ,timeout/2
        ,handle_disconnect/2
        ,terminate/3
        ,code_change/3]).






start_link(Port, Sec) ->
    Host = "127.0.0.1",
    sockerl:start_link_connector_pool(?MODULE
                                     ,Sec
                                     ,Host
                                     ,Port
                                     ,[{connector_count, 5} % 5 connections
                                      ,{connector_debug, [trace]}]).





connector_init(Sec, _Sock) ->
    %% I want to send packet after spending Sec
    %% I will do this in timeout/2
    %% I don't specify {state, _}, my state will be atom 'undefined'
    {ok, [{timeout, Sec * 1000}]}.





handle_packet(_Packet, undefined=_State, SMD) -> % SMD: Sockerl MetaData
    %% Echo server replies my packet
    %% I want to send next packet after spending previous timeout+1
    %% I will do this in timeout/2 too
    {ok, [{timeout, sockerl_metadata:get_timeout(SMD) + 1000}]}.



handle_disconnect(undefined = _State, _SMD) ->
    %% Will exit with reason 'closed_by_remote' if server closes
    %%  connection
    {stop, closed_by_remote}.





timeout(undefined = _State, _SMD) ->
    %% I will send timestamp (in seconds) to server
    {Me, S, _Mi} = os:timestamp(),
    TSBin = erlang:integer_to_binary((Me * 1000000) + S),
    %% I don't change 'timeout' value, process keeps its last value and
    %%  after sending packet if server replies, i will change it (add 1
    %%  second to it) and if server doesn't reply, after that timeout
    %%  This function will called again.
    {ok, [{packet, TSBin}]}.




terminate(_Reason, undefined = _State, _SMD) ->
    ok.





code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
