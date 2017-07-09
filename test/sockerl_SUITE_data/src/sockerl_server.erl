-module(sockerl_server).
-export([start_link/3
        ,start_link/4
        ,listen_init/2
        ,connector_init/2
        ,handle_packet/3
        ,handle_event/3
        ,handle_call/4
        ,handle_cast/3
        ,handle_disconnect/2
        ,code_change/3
        ,timeout/2
        ,srtimeout/2
        ,terminate/3]).






start_link(InitArg, Port, Opts) ->
    sockerl:start_link_server(?MODULE, InitArg, Port, Opts).


start_link(Name, InitArg, Port, Opts) ->
    sockerl:start_link_server({local, Name}
                             ,?MODULE
                             ,InitArg
                             ,Port
                             ,Opts).







listen_init(InitArg, _LSock) ->
    InitArg().



connector_init(InitArg, _CScok) ->
    process_flag(trap_exit, true),
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


handle_disconnect(State, SMD) ->
    make_return(State, {?MODULE, handle_disconnect, erlang:make_ref(), State, SMD}).


code_change(Old, State, Extra) ->
    make_return(State, {?MODULE, code_change, erlang:make_ref(), Old, State, Extra}).




make_return(Pid, Msg) ->
    Ref = erlang:element(3, Msg),
    Pid ! Msg,
    receive
        {Ref, Value} ->
            Value
    end.