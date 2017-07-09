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
%%          socket acceptor documentation.
%% @end
%% ---------------------------------------------------------------------


-module(sockerl_acceptor).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/3
        ,sleep/1
        ,wakeup/1
        ,get_mode/1
        ,change_mode/1]).





%% 'proc_lib' export
-export([init/4]).





%% 'sys' exports:
-export([system_code_change/4
        ,system_continue/3
        ,system_get_state/1
        ,system_replace_state/2
        ,system_terminate/4]).





%% Internal:
-export([accept/2]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





-record(sockerl_acceptor_state_record, {connection_sup
                                       ,listen_socket
                                       ,name
                                       ,mode
                                       ,transporter
                                       ,options
                                       ,active
                                       ,acceptor}).
-define(STATE, sockerl_acceptor_state_record).

-define(DEFAULT_TRANSPORT_MODULE, 'sockerl_tcp_transporter').
-define(SLEEP, 'sleep').
-define(ACCEPT, 'accept').
-define(GET_MODE, 'get_mode').
-define(CHANGE_MODE, 'change_mode').
-define(GEN_CALL_TAG, '$gen_call').

-define(DEFAULT_DEBUG, []).
-define(DEFAULT_ACCEPT_TIMEOUT, 500).
-define(DEFAULT_HANDSHAKE_TIMEOUT, 500).
-define(DEFAULT_MODE, ?ACCEPT).
-define(DEFAULT_FORWARD_RECEIVE_TIMEOUT, 500).





%% ---------------------------------------------------------------------
%% API functions:





-spec
start_link(sockerl_types:start_options()
          ,sockerl_types:socket()
          ,Pool::sockerl_types:name()) ->
    sockerl_types:start_return().
start_link(Opts, ListenSock, ConSup) ->
    proc_lib:start_link(?MODULE
                       ,init
                       ,[Opts
                        ,ListenSock
                        ,ConSup
                        ,erlang:self()]).







-spec
sleep(sockerl_types:name()) ->
    'ok'.
sleep(Acc) ->
    gen_server:call(Acc, ?SLEEP).







-spec
wakeup(sockerl_types:name()) ->
    'ok'.
wakeup(Acc) ->
    gen_server:call(Acc, ?ACCEPT).







-spec
get_mode(sockerl_types:name()) ->
    sockerl_types:acceptor_mode().
get_mode(Acc) ->
    gen_server:call(Acc, ?GET_MODE).







-spec
change_mode(sockerl_types:name()) ->
    sockerl_types:acceptor_mode().
change_mode(Acc) ->
    gen_server:call(Acc, ?CHANGE_MODE).





%% ---------------------------------------------------------------------
%% 'proc_lib' callback:





%% @hidden
init(Opts
    ,ListenSock
    ,ConSup
    ,Parent) ->
    erlang:process_flag(trap_exit, true),
    DbgOpts = sockerl_utils:get_value(acceptor_debug
                                     ,Opts
                                     ,?DEFAULT_DEBUG
                                     ,fun erlang:is_list/1),
    Dbg = sockerl_utils:debug_options(sockerl_acceptor
                                     ,erlang:self()
                                     ,DbgOpts),
    TrMod = sockerl_utils:get_value(transporter
                                       ,Opts
                                       ,?DEFAULT_TRANSPORT_MODULE
                                       ,fun erlang:is_atom/1),
    Mode = sockerl_utils:get_value(acceptor_mode
                                  ,Opts
                                  ,?DEFAULT_MODE
                                  ,fun filter_mode/1),
    Name = erlang:self(),
    {ok, Activity} = sockerl_socket:is_active(TrMod, ListenSock, Opts),
    State = #?STATE{connection_sup = ConSup
                   ,listen_socket = ListenSock
                   ,name = erlang:self()
                   ,mode = Mode
                   ,transporter = TrMod
                   ,options = Opts
                   ,active = Activity},
    proc_lib:init_ack(Parent, {ok, erlang:self()}),
    loop(Parent
        ,debug(Name, Dbg, {start, TrMod, ListenSock, ConSup, Mode})
        ,State).





%% ---------------------------------------------------------------------
%% 'sys' callbacks:





%% @hidden
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.







%% @hidden
system_continue(Parent, Dbg, State) ->
    loop(Parent, Dbg, State).







%% @hidden
system_get_state(State) ->
    {ok, State}.







%% @hidden
system_replace_state(StateFun, State) ->
    State2 = StateFun(State),
    {ok, State2, State2}.







%% @hidden
system_terminate(Reason, _Parent, Dbg, State) ->
    terminate(Dbg, State, Reason).





%% ---------------------------------------------------------------------
%% Internal functions:





loop(Parent, Dbg, #?STATE{mode = accept, acceptor = undefined}=State) ->
    Acceptor = erlang:spawn_link(?MODULE, accept, [Dbg, State]),
    {Dbg2, State2} =
        receive
            Msg ->
                process_message(Parent
                               ,Dbg
                               ,State#?STATE{acceptor = Acceptor}
                               ,Msg)
        end,
    loop(Parent, Dbg2, State2);
loop(Parent, Dbg, State) ->
    {Dbg2, State2} =
        receive
            Msg ->
                process_message(Parent, Dbg, State, Msg)
        end,
    loop(Parent, Dbg2, State2).









process_message(_Parent
               ,Dbg
               ,#?STATE{acceptor = Pid}=State
               ,{'EXIT', Pid, Msg}) ->
    case Msg of
        ok ->
            {Dbg, State#?STATE{acceptor = undefined}};
        {ok, Dbg2} ->
            {Dbg2, State#?STATE{acceptor = undefined}};
        {error, Reason} ->
            terminate(Dbg, State, Reason);
        Other ->
            terminate(Dbg, State, {unknown_acceptor_msg, Other})
    end;
process_message(_Parent
               ,Dbg
               ,#?STATE{name=Name}=State
               ,{?GEN_CALL_TAG, From, Msg}) ->
    process_request(debug(Name, Dbg, {?GEN_CALL_TAG, From, Msg})
                   ,State
                   ,From
                   ,Msg);
process_message(Parent, Dbg, State, {system, From, Msg}) ->
    sys:handle_system_msg(Msg, From, Parent, ?MODULE, Dbg, State);
process_message(Parent, Dbg, State, {'EXIT', Parent, Reason}) ->
    terminate(Dbg, State, Reason);
process_message(_Parent
               ,Dbg
               ,#?STATE{name= Name}=State
               ,Msg) ->
    {debug(Name, Dbg, {in, Msg}), State}.







process_request(Dbg, #?STATE{name = Name}=State, From, ?SLEEP) ->
    {reply(Name, debug(Name, Dbg, {?CHANGE_MODE, ?SLEEP}), From, ok)
    ,State#?STATE{mode = ?SLEEP}};
process_request(Dbg, #?STATE{name = Name}=State, From, ?ACCEPT) ->
    {reply(Name, debug(Name, Dbg, {?CHANGE_MODE, ?ACCEPT}), From, ok)
    ,State#?STATE{mode = ?ACCEPT}};
process_request(Dbg
               ,#?STATE{name = Name, mode = Mode}=State
               ,From
               ,?GET_MODE) ->
    {reply(Name, debug(Name, Dbg, ?GET_MODE), From, Mode), State};
process_request(Dbg
               ,#?STATE{name = Name, mode = Mode}=State
               ,From
               ,?CHANGE_MODE) ->
    Mode2 =
        case Mode of
            ?SLEEP ->
                ?ACCEPT;
            ?ACCEPT ->
                ?SLEEP
        end,
    {reply(Name, debug(Name, Dbg, {?CHANGE_MODE, Mode2}), From, Mode2)
    ,State#?STATE{mode = Mode2}};

process_request(Dbg, #?STATE{name = Name}=State, From, Other) ->
    {reply(Name, Dbg, From, {error, {unknown_call, [{call, Other}]}})
    ,State}.








%% @hidden
accept(Dbg, #?STATE{connection_sup = Pid
                   ,listen_socket = ListenSock
                   ,name =Name
                   ,transporter = TrMod
                   ,options = Opts
                   ,active = Active}) ->
    case sockerl_socket:accept(TrMod, ListenSock, Opts) of
        {ok, Sock} ->
            Dbg2 = debug(Name, Dbg, {accept, Sock}),
            case sockerl_connector_sup:add(Pid, Sock) of
                {ok, Pid2} ->
                    _ = sockerl_socket:controlling_process(TrMod
                                                      ,Sock
                                                      ,Pid2
                                                      ,Opts),
                    if
                        Active ->
                            forward(Pid2);
                        true ->
                            ok
                    end,
                    erlang:exit({ok
                                ,debug(Name
                                      ,Dbg2
                                      ,{start_connection
                                       ,Sock
                                       ,Pid2})});
                {error, Reason} ->
                    erlang:exit({ok
                                ,debug(Name
                                      ,Dbg2
                                      ,{start_connection_error
                                       ,Sock
                                       ,Reason})});
                ignore ->
                    erlang:exit({ok
                                ,debug(Name
                                      ,Dbg2
                                      ,{start_connection_ignore
                                       ,Sock})})
            end;
        {error, {socket_accept, [{reason, timeout}|_ErrorParams]}} ->
            erlang:exit({ok, Dbg});
        {error
        ,{socket_accept
         ,[{reason, {handshake, timeout}}|_ErrorParams]}} ->
            erlang:exit(ok);
        {error, Reason} ->
            erlang:exit({error, Reason})
    end.







forward(Pid) ->
    receive
        Msg ->
            Pid ! Msg,
            forward(Pid)
    after ?DEFAULT_FORWARD_RECEIVE_TIMEOUT ->
        ok
    end.







terminate(Dbg
         ,#?STATE{transporter = TrMod
                 ,options = Opts
                 ,name = Name
                 ,listen_socket = ListenSock}=State
         ,Reason) ->
    _ = sockerl_socket:close(TrMod, ListenSock, Opts),
    error_logger:format("** Sockerl acceptor \"~p\" terminating \n** Re"
    "ason for termination == \"~p\"~n** State == \""
    "~p\"~n"
                       ,[Name, Reason, State]),
    sys:print_log(Dbg),
    erlang:exit(Reason).







reply(Name, Dbg, {Pid, Tag}, Msg) ->
    catch Pid ! {Tag, Msg},
    debug(Name, Dbg, {out, Msg, Pid}).







debug(_Name, [], _Event) ->
    [];
debug(Name, Dbg, Event) ->
    sys:handle_debug(Dbg, fun handle_debug/3, Name, Event).







handle_debug(IODev, {accept, Sock}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" accepted socket \"~p\"~n"
             ,[Name, Sock]);

handle_debug(IODev, {start_connection, Sock, Pid}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" gave socket \"~p\" to conn"
              "ection handler \"~p\"~n"
             ,[Name, Sock, Pid]);

handle_debug(IODev, {start_connection_error, Sock, Reason}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" could not start connection"
              " handler for socket \"~p\" with reason \"~p\"~n"
             ,[Name, Sock, Reason]);

handle_debug(IODev, {start_connection_ignore, Sock}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" starting new connection ha"
              "ndler for socket \"~p\" was ignored~n"
             ,[Name, Sock]);

handle_debug(IODev, {out, Msg, Pid}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" sent message \"~p\" to \"~"
              "p\" ~n"
             ,[Name, Msg, Pid]);

handle_debug(IODev, {?GEN_CALL_TAG, {Pid, _Tag}, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" got request \"~p\" from \""
              "~p\"~n"
             ,[Name, Msg, Pid]);

handle_debug(IODev, {?CHANGE_MODE, Mode}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" changed mod to \"~p\"~n"
             ,[Name, Mode]);

handle_debug(IODev, ?GET_MODE, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" got request for getting it"
              "s mode~n"
             ,[Name]);

handle_debug(IODev, {in, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" got message \"~p\"~n"
             ,[Name, Msg]);

handle_debug(IODev
            ,{start
             ,TrMod
             ,ListenSock
             ,ConSup
             ,Mode}
            ,Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" started for listen socket "
              "\"~p\" with options:~n \ttransporter module: '~p'~n \tpo"
              "ol pid: ~p~n \tmode: '~p'~n"
             ,[Name
              ,ListenSock
              ,TrMod
              ,ConSup
              ,Mode]);

handle_debug(IODev, Event, Name) ->
    io:format(IODev
             ,"*DBG* Sockerl acceptor \"~p\" got event \"~p\"~n"
             ,[Name, Event]).







filter_mode(?ACCEPT) ->
    ok;
filter_mode(?SLEEP) ->
    ok;
filter_mode(Other) ->
    {error, {mode, [{mode, Other}]}}.