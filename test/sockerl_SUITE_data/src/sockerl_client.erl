-module(sockerl_client).
-export([start_link/4
        ,start_link/5
        ,connector_init/2
        ,handle_packet/3
        ,handle_event/3
        ,handle_call/4
        ,handle_cast/3
        ,code_change/3
        ,timeout/2
        ,srtimeout/2
        ,terminate/3]).






start_link(InitArg, Host, Port, Opts) ->
    sockerl:start_link_connector(?MODULE, InitArg, Host, Port, Opts).


start_link(Name, InitArg, Host, Port, Opts) ->
    sockerl:start_link_connector({local, Name}
                             ,?MODULE
                             ,InitArg
                             ,Host
                             ,Port
                             ,Opts).







connector_init(InitArg, _CScok) ->
    erlang:process_flag(trap_exit, true),
    InitArg().




handle_packet(Packet, State, SMD) ->
    make_return(State, {?MODULE, handle_packet, erlang:make_ref(), Packet, State, SMD}).




timeout(State, SMD) ->
    make_return(State, {?MODULE, timeout, erlang:make_ref(), State, SMD}).




srtimeout(State, SMD) ->
    make_return(State, {?MODULE, srtimeout, erlang:make_ref(), State, SMD}).



handle_call(Call, From, State, SMD) ->
    make_return(State, {?MODULE, handle_call, erlang:make_ref(), Call, From, State, SMD}).


handle_cast(Cast, State, SMD) ->
    make_return(State, {?MODULE, handle_cast, erlang:make_ref(), Cast, State, SMD}).


handle_event(Event, State, SMD) ->
    make_return(State, {?MODULE, handle_event, erlang:make_ref(), Event, State, SMD}).



terminate(Reason, State, SMD) ->
    make_return(State, {?MODULE, terminate, erlang:make_ref(), Reason, State, SMD}).


code_change(Old, State, Extra) ->
    make_return(State, {?MODULE, code_change, erlang:make_ref(), Old, State, Extra}).




make_return(Pid, Msg) ->
    Ref = erlang:element(3, Msg),
    Pid ! Msg,
    receive
        {Ref, Value} ->
            Value
    end.