%%% --------------------------------------------------------------------
%%% BSD 3-Clause License
%%%
%%% Copyright (c) 2016-2017, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% --------------------------------------------------------------------
%% @author  Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version 17.7.10
%% @hidden
%% @doc
%%          Connector documentation.
%% @end
%% ---------------------------------------------------------------------


-module(sockerl_connector).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/4
        ,start_link/5
        ,start_link/6
        ,send_sync/2
        ,send_sync/3
        ,send_async/2
        ,stop/1
        ,stop/2]).





%% 'gen' callback:
-export([init_it/6]).





%% 'sys' callbacks:
-export([system_terminate/4
        ,system_replace_state/2
        ,system_get_state/1
        ,system_continue/3
        ,system_code_change/4]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





-record(sockerl_connector_state_record, {name
                                        ,data
                                        ,active
                                        ,module
                                        ,metadata}).
-define(STATE, sockerl_connector_state_record).





%% Dependencies:
%%  #sockerl_metadata{}
-include("internal/sockerl_metadata.hrl").
-define(SMD, sockerl_metadata).





-define(DEFAULT_TERMINATE_TIMEOUT, 5000).
-define(GEN_CALL_TAG, '$gen_call').
-define(GEN_CAST_TAG, '$gen_cast').
-define(GEN_EVENT_TAG, '$gen_event').
-define(GEN_SOCKERL_TAG, '$gen_sockerl').

-define(DEFAULT_LENGHT, 0).
-define(DEFAULT_TIMEOUT, infinity).
-define(DEFAULT_SRTIMEOUT, infinity).

-define(DEFAULT_START_OPTIONS, []).
-define(DEFAULT_SOCKET_OPTIONS, []).
-define(DEFAULT_SSL_FLAG, false).
-define(DEFAULT_CONNECT_TIMEOUT, 3000).
-define(DEFAULT_DEBUG, []).
-define(DEFAULT_TRANSPORT_MODULE, 'sockerl_tcp_transporter').





%% ---------------------------------------------------------------------
%% API:





-spec
start_link(module()
          ,term()
          ,sockerl_types:socket() | sockerl_types:host()
          ,sockerl_types:options() | sockerl_types:port_number()) ->
    sockerl_types:start_return().
%% @doc
%%      starts and links a socket connection handler process.
%% @end
start_link(Mod
          ,InitArg
          ,Host
          ,Port) when erlang:is_atom(Mod) andalso
                      erlang:is_integer(Port) ->
    gen:start(?MODULE
             ,link
             ,Mod
             ,{InitArg, Host, Port}
             ,?DEFAULT_START_OPTIONS);
start_link(Mod
          ,InitArg
          ,Opts
          ,Sock) when erlang:is_atom(Mod) andalso
    erlang:is_list(Opts) ->
    gen:start(?MODULE, link, Mod, {InitArg, Sock}, Opts).







-spec
start_link(module() | sockerl_types:register_name()
          ,term() | module()
          ,sockerl_types:host()          |
           term()                        |
           sockerl_types:start_options()
          ,sockerl_types:port_number() | sockerl_types:host()
          ,sockerl_types:options() | sockerl_types:port_number()) ->
    sockerl_types:start_return().
start_link(Mod
          ,InitArg
          ,Host
          ,Port
          ,Opts) when erlang:is_atom(Mod) andalso
                      erlang:is_integer(Port) andalso
                      erlang:is_list(Opts) ->
    gen:start(?MODULE, link, Mod, {InitArg, Host, Port}, Opts);
%% Will called by calling sockerl_connector_sup:add/3
start_link(Mod
          ,InitArg
          ,Opts
          ,Host
          ,Port) when erlang:is_atom(Mod) andalso
                      erlang:is_integer(Port) andalso
                      erlang:is_list(Opts) ->
    gen:start(?MODULE, link, Mod, {InitArg, Host, Port}, Opts);
start_link(Name
          ,Mod
          ,InitArg
          ,Host
          ,Port) when erlang:is_tuple(Name) andalso
                      erlang:is_atom(Mod) andalso
                      erlang:is_integer(Port) ->
    gen:start(?MODULE
             ,link
             ,Name
             ,Mod
             ,{InitArg, Host, Port}
             ,?DEFAULT_START_OPTIONS).







-spec
start_link(sockerl_types:register_name()
          ,module()
          ,term()
          ,sockerl_types:host()
          ,sockerl_types:port_number()
          ,sockerl_types:options()) ->
    sockerl_types:start_return().
start_link(Name
          ,Mod
          ,InitArg
          ,Host
          ,Port
          ,Opts) when erlang:is_tuple(Name) andalso
                      erlang:is_atom(Mod) andalso
                      erlang:is_integer(Port) andalso
                      erlang:is_list(Opts) ->
    gen:start(?MODULE
             ,link
             ,Name
             ,Mod
             ,{InitArg, Host, Port}
             ,Opts).







-spec
send_sync(sockerl_types:name(), sockerl_types:packet()) ->
    'ok' | sockerl_types:error().
send_sync(Con, Packet) ->
    case catch gen:call(Con, ?GEN_SOCKERL_TAG, {send, Packet}) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            erlang:exit({Reason, [{module, ?MODULE}
                                 ,{function, send_sync}
                                 ,{arguments, [Con, Packet]}]})
    end.







-spec
send_sync(sockerl_types:name(), sockerl_types:packet(), timeout()) ->
    'ok' | sockerl_types:error().
send_sync(Con, Packet, Timeout) ->
    case catch gen:call(Con
                       ,?GEN_SOCKERL_TAG
                       ,{send, Packet}
                       ,Timeout) of
        {ok, Res} ->
            Res;
        {'EXIT', Reason} ->
            erlang:exit({Reason, [{module, ?MODULE}
                                 ,{function, send_sync}
                                 ,{arguments, [Con, Packet, Timeout]}]})
    end.








-spec
send_async(sockerl_types:name(), sockerl_types:packet()) ->
    'ok'.
send_async(Con, Packet) ->
    catch Con ! {?GEN_SOCKERL_TAG, {send, Packet}},
    ok.







-spec
stop(sockerl_types:name()) ->
    ok.
stop(Con) ->
    proc_lib:stop(Con, normal, ?DEFAULT_TERMINATE_TIMEOUT).







-spec
stop(sockerl_types:name(), any()) ->
    ok.
stop(Con, Reason) ->
    proc_lib:stop(Con, Reason, ?DEFAULT_TERMINATE_TIMEOUT).





%% ---------------------------------------------------------------------
%% 'gen' callback:





%% @hidden
init_it(Starter
       ,Parent
       ,Name
       ,Mod
       ,{InitArg, Host, Port}
       ,Opts) ->
    TrMod = sockerl_utils:get_value(transporter
                                   ,Opts
                                   ,?DEFAULT_TRANSPORT_MODULE
                                   ,fun erlang:is_atom/1),
    case sockerl_socket:connect(TrMod
                               ,Host
                               ,Port
                               ,Opts) of
        {ok, Sock} ->
            init_it(Starter
                   ,Parent
                   ,Name
                   ,Mod
                   ,{InitArg, Sock}
                   ,Opts
                   ,{Host, Port});
        {error, Reason}=Error ->
            proc_lib:init_ack(Starter, Error),
            erlang:exit(Reason)
    end;
init_it(Starter, Parent, Name, Mod, {InitArg, Sock}, Opts) ->
    init_it(Starter
           ,Parent
           ,Name
           ,Mod
           ,{InitArg, Sock}
           ,Opts
           ,{undefined, undefined}).

init_it(Starter, Parent, Name, Mod, {InitArg, Sock}, Opts, Addr) ->
    TrMod = sockerl_utils:get_value(transporter
                                   ,Opts
                                   ,?DEFAULT_TRANSPORT_MODULE
                                   ,fun erlang:is_atom/1),
    DbgOpts = sockerl_utils:get_value(connector_debug
                                     ,Opts
                                     ,?DEFAULT_DEBUG
                                     ,fun erlang:is_list/1),
    Dbg = sockerl_utils:debug_options(?MODULE, Name, DbgOpts),
    case sockerl_socket:is_active(TrMod, Sock, Opts) of
        {ok, Active} ->
            SMD = sockerl_metadata:wrap(Sock
                                       ,?DEFAULT_TIMEOUT
                                       ,?DEFAULT_SRTIMEOUT
                                       ,?DEFAULT_LENGHT
                                       ,TrMod
                                       ,Opts
                                       ,undefined
                                       ,undefined
                                       ,Addr),
            State = #?STATE{name = Name
                           ,data = undefined
                           ,module = Mod
                           ,active = Active
                           ,metadata = SMD},
            case run_callback2(Dbg
                              ,State
                              ,Mod
                              ,connector_init
                              ,[InitArg, SMD]) of
                {ok, Dbg3, State2} ->
                    proc_lib:init_ack(Starter, {ok, erlang:self()}),
                    loop(Parent, Dbg3, State2);

                {close, _Dbg3, _State2} ->
                    _ = sockerl_socket:close(TrMod, Sock, Opts),
                    proc_lib:init_ack(Starter, {error, closed}),
                    erlang:exit(normal);

                {stop, _Dbg3, _State2, Reason} ->
                    _ = sockerl_socket:close(TrMod, Sock, Opts),
                    proc_lib:init_ack(Starter, {error, Reason}),
                    erlang:exit(Reason);

                {error
                ,{bad_return_value, [{returned_value, ignore}|_]}} ->
                    _ = sockerl_socket:close(TrMod, Sock, Opts),
                    proc_lib:init_ack(Starter, ignore),
                    erlang:exit(normal);

                {error, Reason}=Error ->
                    _ = sockerl_socket:close(TrMod, Sock, Opts),
                    proc_lib:init_ack(Starter, Error),
                    erlang:exit(Reason)
            end;
        {error, Reason}=Error ->
            _ = sockerl_socket:close(TrMod, Sock, Opts),
            proc_lib:init_ack(Starter, Error),
            erlang:exit(Reason)
    end.





%% ---------------------------------------------------------------------
%% 'sys' callbacks:





%% @hidden
system_code_change(#?STATE{module = Mod, data = Data}=State
                  ,_Module
                  ,OldVsn
                  ,Extra) ->
    case Mod:code_change(OldVsn, Data, Extra) of
        {ok, Data2} ->
            {ok, State#?STATE{data = Data2}};
        Other ->
            Other
    end.







%% @hidden
system_continue(Parent, Dbg, State) ->
    loop(Parent, Dbg, State).







%% @hidden
system_get_state(#?STATE{data=Data}) ->
    {ok, Data}.







%% @hidden
system_replace_state(StateFun, #?STATE{data=Data}=State) ->
    Data2 = StateFun(Data),
    {ok, Data2, State#?STATE{data=Data2}}.







%% @hidden
system_terminate(Reason, _Parent, Dbg, State) ->
    terminate(Dbg, State, Reason).





%% ---------------------------------------------------------------------
%% Internal functions:





run_callback2(Dbg, #?STATE{metadata = SMD}=State, Mod, Func, Args) ->
    Ret2 =
        case catch erlang:apply(Mod, Func, Args) of
            ok ->
                {ok
                ,Dbg
                ,State#?STATE{metadata =
                              SMD#?SMD{last_callback = Func}}};
            {ok, Opts} ->
                get_options(Dbg, State, Opts);
            close ->
                {close
                ,Dbg
                ,State#?STATE{metadata =
                              SMD#?SMD{last_callback = Func}}};
            {close, Opts} ->
                case get_options(Dbg, State, Opts) of
                    {ok, Dbg2, State2}  ->
                        {close
                        ,Dbg2
                        ,State2#?STATE{metadata =
                                       SMD#?SMD{last_callback = Func}}};
                    {error, _Reason}=Error ->
                        Error
                end;
            {stop, Reason} ->
                {stop
                ,Dbg
                ,State#?STATE{metadata = SMD#?SMD{last_callback = Func}}
                ,Reason};
            {stop, Reason, Opts} ->
                case get_options(Dbg, State, Opts) of
                    {ok, Dbg2, State2}  ->
                        {stop
                        ,Dbg2
                        ,State2#?STATE{metadata =
                                       SMD#?SMD{last_callback = Func}}
                        ,Reason};
                    {error, _Reason}=Error ->
                        Error
                end;
            {'EXIT', Reason} ->
                {error, {callback_crash, [{reason, Reason}]}};
            Other ->
                {error, {callback_bad_return_value
                        ,[{returned_value, Other}]}}
        end,
    case Ret2 of
        {error, {Reason2, ErrorParams}} ->
            {error, {Reason2, ErrorParams ++ [{module, Mod}
                                             ,{function, Func}
                                             ,{arguments, Args}]}};
        _ ->
            Ret2
    end.







get_options(Dbg, State, [{state, Data} | Opts]) ->
    get_options(Dbg, State#?STATE{data = Data}, Opts);

get_options(Dbg
           ,#?STATE{metadata = SMD}=State
           ,[{timeout, Timeout}
            |Opts]) when erlang:is_integer(Timeout) ->
    if
        Timeout >= 0 ->
            get_options(Dbg
                       ,State#?STATE{metadata =
                                     SMD#?SMD{timeout = Timeout}}
                       ,Opts);
        true ->
            {error, {timeout_range, [{timeout, Timeout}]}}
    end;

get_options(Dbg
           ,#?STATE{metadata = SMD}=State
           ,[{timeout, infinity} |Opts]) ->
    get_options(Dbg
               ,State#?STATE{metadata = SMD#?SMD{timeout = infinity}}
               ,Opts);

get_options(Dbg
           ,#?STATE{metadata = SMD}=State
           ,[{srtimeout, SRTimeout}
            |Opts]) when erlang:is_integer(SRTimeout) ->
    if
        SRTimeout >= 0 ->
            get_options(Dbg
                       ,State#?STATE{metadata =
                                     SMD#?SMD{srtimeout = SRTimeout}}
                       ,Opts);
        true ->
            {error, {srtimeout_range, [{srtimeout, SRTimeout}]}}
    end;

get_options(Dbg
           ,#?STATE{metadata = SMD}=State
           ,[{srtimeout, infinity} |Opts]) ->
    get_options(Dbg
               ,State#?STATE{metadata = SMD#?SMD{srtimeout = infinity}}
               ,Opts);

get_options(Dbg
           ,#?STATE{name = Name
                   ,metadata = #?SMD{socket = Sock
                                    ,transporter = TrMod
                                    ,options = Opts}}=State
           ,[{packet, Pkt} | Opts2]) ->
    case socket_send(TrMod, Sock, Name, Dbg, Pkt, Opts) of
        {ok, Dbg2} ->
            get_options(Dbg2, State, Opts2);
        {error, _Reason}=Error ->
            Error
    end;

get_options(Dbg
           ,#?STATE{metadata = SMD}=State
           ,[{length, Len} |Opts]) when erlang:is_integer(Len) ->
    get_options(Dbg
               ,State#?STATE{metadata = SMD#?SMD{length = Len}}
               ,Opts);

get_options(Dbg
           ,#?STATE{name = Name}=State
           ,[{reply, From, Msg} |Opts]) ->
    get_options(reply(Name, Dbg, From, Msg), State, Opts);

get_options(Dbg, State, []) ->
    {ok, Dbg, State};

get_options(Dbg
           ,#?STATE{metadata = SMD}=State
           ,[{transporter, TrMod}|Opts]) when erlang:is_atom(TrMod) ->
    get_options(Dbg
               ,State#?STATE{metadata = SMD#?SMD{transporter = TrMod}}
               ,Opts);

get_options(Dbg
           ,#?STATE{metadata = SMD}=State
           ,[{socket, Sock}|Opts]) ->
    get_options(Dbg
               ,State#?STATE{metadata = SMD#?SMD{socket = Sock}}
               ,Opts);

get_options(Dbg
           ,#?STATE{metadata = #?SMD{transporter = TrMod
                                    ,socket = Sock
                                    ,options = Opts}}=State
           ,[{setopts, SockOpts}
            |Opts2]) when erlang:is_list(SockOpts) ->
    case sockerl_utils:filter_socket_options(SockOpts) of
        ok ->
            case sockerl_socket:setopts(TrMod, Sock, SockOpts, Opts) of
                ok ->
                    case sockerl_socket:is_active(TrMod, Sock, Opts) of
                        {ok, Bool} ->
                            get_options(Dbg
                                       ,State#?STATE{active = Bool}
                                       ,Opts2);
                        {error, _Reason}=Error ->
                            Error
                    end;
                {error, _Reason}=Error ->
                    Error
            end;
        {error, _Reason}=Error ->
            Error
    end;

get_options(_Dbg, _State, [{timeout, Timeout} | _Opts]) ->
    {error, {timeout_type, [{timeout, Timeout}]}};

get_options(_Dbg, _State, [{srtimeout, SRTimeout} | _Opts]) ->
    {error
    ,{srtimeout_type, [{srtimeout, SRTimeout}]}};

get_options(_Dbg, _State, [{length, Len} | _Opts]) ->
    {error, {length_type, [{length, Len}]}};

get_options(_Dbg, _State, [{transporter, TrMod}|_Opts]) ->
    {error, {transporter_type, [{transporter, TrMod}]}};

get_options(_Dbg, _State, [{setopts, Opts}|_Opts]) ->
    {error, {setopts_type, [{setopts, Opts}]}};

get_options(_Dbg, _State, [{Key, Val}|_Opts]) ->
    {error, {unknown_option, [{tag, Key}, {value, Val}]}};

get_options(_Dbg, _State, [Opt|_Opts]) ->
    {error, {option_format, [{option, Opt}]}};

get_options(_Dbg, _State ,Opts) ->
    {error, {options_type, [{options, Opts}]}}.







loop(Parent
    ,Dbg
    ,#?STATE{name = Name
            ,active = false
            ,metadata = #?SMD{socket =Sock
                             ,length = Len
                             ,srtimeout = SRTimeout
                             ,transporter = TrMod
                             ,options = Opts}}=State) when Len >= 0 ->
    case socket_receive(TrMod, Sock, Name, Dbg, Len, SRTimeout, Opts) of
        {ok, Dbg2, Packet} ->
            {Dbg3, State2} = run_callback(Dbg2
                                         ,State
                                         ,handle_packet
                                         ,{Packet}),
            do_receive(Parent, Dbg3, State2);
        {error, {socket_recv, [{reason, timeout}|_ErrorParams]}} ->
            {Dbg2, State2} = run_callback(Dbg
                                         ,State
                                         ,srtimeout
                                         ,{}),
            do_receive(Parent, Dbg2, State2);
        {error, {socket_recv, [{reason, closed}|_ErrorParams]}} ->
            {Dbg2, State2} = run_callback(Dbg
                                         ,State
                                         ,handle_disconnect
                                         ,{}),
            terminate(Dbg2, State2, normal);
        {error, Reason} ->
            terminate(Dbg, State, Reason)
    end;
loop(Parent, Dbg, State) ->
    do_receive(Parent, Dbg, State).







run_callback(Dbg
            ,#?STATE{module = Mod
                    ,data= Data
                    ,metadata = SMD}=State
            ,Callback
            ,Args) ->
    Args2 =
        case Args of
            {} ->
                [Data, SMD];
            {Arg} ->
                [Arg, Data, SMD];
            {Arg1, Arg2} ->
                [Arg1, Arg2, Data, SMD]
        end,
    case run_callback2(Dbg, State, Mod, Callback, Args2) of
        {ok, Dbg2, State2} ->
            {Dbg2, State2};
        {close, Dbg2, State2} ->
            terminate(Dbg2, State2, normal);
        {stop, Dbg2, State2, Reason} ->
            terminate(Dbg2, State2, Reason);
        {error, Reason} ->
            terminate(Dbg, State, Reason)
    end.







socket_receive(TrMod, Sock, Name, Dbg, Len, SRTimeout, Opts) ->
    case sockerl_socket:recv(TrMod, Sock, Len, SRTimeout, Opts) of
        {ok, Packet} ->
            {ok, debug(Name, Dbg, {socket_in, Packet}), Packet};
        {error, _Reason}=Error ->
            Error
    end.







socket_send(TrMod, Sock, Name,  Dbg, Packet, Opts) ->
    case sockerl_socket:send(TrMod, Sock, Packet, Opts) of
        ok ->
            {ok, debug(Name, Dbg, {socket_out, Packet})};
        {error, _Reason}=Error ->
            Error
    end.







do_receive(Parent
          ,Dbg
          ,#?STATE{metadata = #?SMD{timeout = Timeout}=SMD}=State) ->
    receive
        Msg ->
            process_message(Parent
                           ,Dbg
                           ,State#?STATE{metadata =
                                         SMD#?SMD{last_message = Msg}}
                           ,Msg)
    after Timeout ->
        {Dbg2, State2} = run_callback(Dbg, State, timeout, {}),
        loop(Parent, Dbg2, State2)
    end.







process_message(Parent
               ,Dbg
               ,#?STATE{name=Name}=State
               ,{?GEN_SOCKERL_TAG, From, Req}=Msg) ->
    {Dbg2, State2} = process_sync_request(debug(Name, Dbg, Msg)
                                         ,State
                                         ,From
                                         ,Req),
    loop(Parent, Dbg2, State2);

process_message(Parent
               ,Dbg
               ,#?STATE{name=Name}=State
               ,{?GEN_SOCKERL_TAG, Req}=Msg) ->
    {Dbg2, State2} = process_async_request(debug(Name, Dbg, Msg)
                                         ,State
                                         ,Req),
    loop(Parent, Dbg2, State2);

process_message(Parent
               ,Dbg
               ,#?STATE{name= Name}=State
               ,{?GEN_CALL_TAG, From, Req}=Msg) ->
    {Dbg2, State2} = run_callback(debug(Name, Dbg, Msg)
                                 ,State
                                 ,handle_call
                                 ,{Req, From}),
    loop(Parent, Dbg2, State2);

process_message(Parent
               ,Dbg
               ,#?STATE{name= Name}=State
               ,{?GEN_CAST_TAG, Msg2}=Msg) ->
    {Dbg2, State2} = run_callback(debug(Name, Dbg, Msg)
                                 ,State
                                 ,handle_cast
                                 ,{Msg2}),
    loop(Parent, Dbg2, State2);

process_message(Parent
               ,Dbg
               ,#?STATE{name= Name}=State
               ,{?GEN_EVENT_TAG, Event}=Msg) ->
    {Dbg2, State2} = run_callback(debug(Name, Dbg, Msg)
                                 ,State
                                 ,handle_event
                                 ,{Event}),
    loop(Parent, Dbg2, State2);

process_message(Parent, Dbg, State, {system, From, Msg}) ->
    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Dbg, State);

process_message(Parent, Dbg, State, {'EXIT', Parent, Reason}) ->
    terminate(Dbg, State, Reason);

process_message(Parent
               ,Dbg
               ,#?STATE{name = Name
                       ,metadata = #?SMD{transporter = TrMod
                                        ,options = Opts}}=State
               ,Msg) ->
    case sockerl_socket:is_socket_message(TrMod, Msg, Opts) of
        {ok, Packet} ->
            {Dbg2, State2} = run_callback(debug(Name
                                               ,Dbg
                                               ,{socket_in, Packet})
                                         ,State
                                         ,handle_packet
                                         ,{Packet}),
            loop(Parent, Dbg2, State2);
        {error, {socket_check_message, [{reason, closed}|_]}} ->
            {Dbg2, State2} = run_callback(Dbg
                                         ,State
                                         ,handle_disconnect
                                         ,{}),
            terminate(Dbg2, State2, normal);
        {error, Reason} ->
            terminate(Dbg, State, Reason);
        false ->
            {Dbg2, State2} = run_callback(debug(Name, Dbg, {in, Msg})
                                         ,State
                                         ,handle_info
                                         ,{Msg}),
            loop(Parent, Dbg2, State2)
    end.







process_sync_request(Dbg
                    ,#?STATE{name = Name
                            ,metadata = #?SMD{socket = Sock
                                             ,transporter = TrMod
                                             ,options = Opts}}=State
                    ,From, {send, Packet}) ->
    case socket_send(TrMod, Sock, Name,  Dbg, Packet, Opts) of
        {ok, Dbg2} ->
            {reply(Name, Dbg2, From, ok), State};
        {error, Reason}=Error ->
            terminate(reply(Name, Dbg, From, Error)
                     ,State
                     ,Reason)
    end;
process_sync_request(Dbg, #?STATE{name = Name}=State, From, Other) ->
    {reply(Name
          ,Dbg
          ,From
          ,{error, {unknown_request, [{request, Other}]}})
    ,State}.







process_async_request(Dbg
                     ,#?STATE{name = Name
                             ,metadata = #?SMD{socket = Sock
                                              ,transporter = TrMod
                                              ,options = Opts}}=State
                     ,{send, Packet}) ->
    case socket_send(TrMod, Sock, Name,  Dbg, Packet, Opts) of
        {ok, Dbg2} ->
            {Dbg2, State};
        {error, Reason} ->
            terminate(Dbg, State, Reason)
    end;
process_async_request(Dbg, State, Other) ->
    terminate(Dbg, State, {unknown_request, [{request, Other}]}).







reply(Name, Dbg, {Pid, Tag}=Client, Msg) ->
        catch Pid ! {Tag, Msg},
    debug(Name, Dbg, {out, Client, Msg});
reply(Name, Dbg, Pid, Msg) ->
    debug(Name, Dbg, {out, Pid, Msg}).







terminate(Dbg
         ,#?STATE{module = Mod
                 ,data = Data
                 ,name = Name
                 ,metadata = #?SMD{socket = Sock
                                  ,transporter = TrMod
                                  ,options = Opts}=SMD}
         ,Reason) ->
    Reason2 =
        case catch Mod:terminate(Reason, Data, SMD) of
            {'EXIT', Reason3} ->
                {Reason, Reason3};
            _Other ->
                Reason
        end,
    error_logger:format("** Sockerl connector \"~p\" terminating \n** "
                        "Reason for termination == \"~p\"~n** State == "
                        "\"~p\"~n"
                       ,[Name, Reason2, Data]),
    _ = sockerl_socket:close(TrMod, Sock, Opts),
    sys:print_log(Dbg),
    erlang:exit(Reason2).







debug(_Name, [], _Event) ->
    [];
debug(Name, Dbg, Event) ->
    sys:handle_debug(Dbg, fun handle_debug/3, Name, Event).







handle_debug(IODev, {socket_in, Packet}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got packet ~p~n"
             ,[Name, Packet]);

handle_debug(IODev, {socket_out, Packet}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" sent packet ~p~n"
             ,[Name, Packet]);

handle_debug(IODev, {in, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got message \"~s\"~n"
             ,[Name, Msg]);

handle_debug(IODev, {out, {Pid, Tag}, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" sent \"~p\" to \"~p\" wi"
              "th tag \"~p\"~n"
             ,[Name, Msg, Pid, Tag]);

handle_debug(IODev, {out, Pid, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" sent \"~p\" to \"~p\"~n"
             ,[Name, Msg, Pid]);

handle_debug(IODev, {?GEN_CALL_TAG, {Pid, Tag}, Req}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got call \"~p\" from \"~"
              "p\" with tag \"~p\"~n"
             ,[Name, Req, Pid, Tag]);

handle_debug(IODev, {?GEN_CAST_TAG, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got cast \"~p\"~n"
             ,[Name, Msg]);

handle_debug(IODev, {?GEN_EVENT_TAG, Event}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got event \"~p\"~n"
             ,[Name, Event]);

handle_debug(IODev
            ,{?GEN_SOCKERL_TAG, {Pid, Tag}, {send, Pkt}}
            ,Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got synchronous request "
              "for sending packet ~p from \"~p\" with tag \"~p\"~n"
             ,[Name, Pkt, Pid, Tag]);

handle_debug(IODev, {?GEN_SOCKERL_TAG, {send, Pkt}}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got asynchronous request"
              " for sending packet ~p~n"
             ,[Name, Pkt]);

handle_debug(IODev, Event, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl connector \"~p\" got debug event \"~p\"~n"
             ,[Name, Event]).