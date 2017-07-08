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
%% @version
%% @hidden
%% ---------------------------------------------------------------------


-module(sockerl_server_sup).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





-export([start_link/3
        ,start_link/4
        ,start_link/5

        ,fetch_connections/1
        ,fetch_acceptors/1

        ,sleep_acceptors/1
        ,wakeup_acceptors/1
        ,get_acceptor_modes/1

        ,stop/1
        ,stop/2]).





%% 'director' export:
-export([init/1]).





%% ---------------------------------------------------------------------
%% Record & Macros & Includes:





-define(ACCEPTOR_SUP, acceptor_sup).
-define(CONNECTION_SUP, connection_sup).
-define(DEFAULT_SERVER_START_OPTIONS, []).
-define(DEFAULT_ACCEPTOR_COUNT, 1).
-define(DEFAULT_SOCKET_OPTIONS, []).
-define(DEFAULT_SSL_FLAG, false).
-define(DEFAULT_TERMINATE_TIMEOUT, 10*1000). %% 10s
-define(DEFAULT_TRANSPORT_MODULE, 'sockerl_tcp_transporter').





%% ---------------------------------------------------------------------
%% API functions:





-spec
start_link(module(), term(), sockerl_types:port_number()) ->
     sockerl_types:start_return().
start_link(Mod, InitArg, Port) when erlang:is_atom(Mod),
                                    erlang:is_integer(Port) ->
    start_link(Mod, InitArg, Port, ?DEFAULT_SERVER_START_OPTIONS).







-spec
start_link(sockerl_types:register_name() | module()
          ,module() | term()
          ,term() | sockerl_types:port_number()
          ,sockerl_types:port_number() |
           sockerl_types:start_options()) ->
    sockerl_types:start_return().
start_link(Mod, InitArg, Port, Opts) when erlang:is_atom(Mod),
                                          erlang:is_integer(Port),
                                          erlang:is_list(Opts)->
    case director:start_link(?MODULE, {Mod, InitArg, Port, Opts}) of
        {ok, Pid} ->
            continue_starting(Opts, Pid);
        {error, _Reason}=Error ->
            Error;
        ignore ->
            ignore
    end;
start_link(Name, Mod, InitArg, Port) when erlang:is_tuple(Name),
                                          erlang:is_atom(Mod),
                                          erlang:is_integer(Port) ->
    case director:start_link(Name
                            ,?MODULE
                            ,{Mod
                             ,InitArg
                             ,Port
                             ,?DEFAULT_SERVER_START_OPTIONS}) of
        {ok, Pid} ->
            continue_starting(?DEFAULT_SERVER_START_OPTIONS, Pid);
        {error, _Reason}=Error ->
            Error;
        ignore ->
            ignore
    end.







-spec
start_link(sockerl_types:register_name()
          ,module()
          ,term()
          ,sockerl_types:port_number()
          ,sockerl_types:start_options()) ->
    sockerl_types:start_return().
start_link(Name, Mod, InitArg, Port, Opts) when erlang:is_tuple(Name),
                                                erlang:is_atom(Mod),
                                                erlang:is_integer(Port),
                                                erlang:is_list(Opts) ->
    case director:start_link(Name
                            ,?MODULE
                            ,{Mod, InitArg, Port, Opts}) of
        {ok, Pid} ->
            continue_starting(Opts, Pid);
        {error, _Reason}=Error ->
            Error;
        ignore ->
            ignore
    end.







-spec
fetch_acceptors(sockerl_types:name()) ->
    [{pos_integer(), pid()}].
fetch_acceptors(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_SUP),
    sockerl_acceptor_sup:fetch(Pid).







-spec
fetch_connections(sockerl_types:name()) ->
    [] | [{sockerl_types:socket(), pid()}].
fetch_connections(Server) ->
    {ok, Pid} = director:get_pid(Server, ?CONNECTION_SUP),
    sockerl_server_connection_sup:fetch(Pid).







-spec
sleep_acceptors(sockerl_types:name()) ->
    'ok'.
sleep_acceptors(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_SUP),
    sockerl_acceptor_sup:sleep(Pid).







-spec
wakeup_acceptors(sockerl_types:name()) ->
    'ok'.
wakeup_acceptors(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_SUP),
    sockerl_acceptor_sup:wakeup(Pid).







-spec
get_acceptor_modes(sockerl_types:name()) ->
    'sleep' | 'accept' | [{pos_integer(), 'sleep' | 'accept'}].
get_acceptor_modes(Server) ->
    {ok, Pid} = director:get_pid(Server, ?ACCEPTOR_SUP),
    sockerl_acceptor_sup:get_mode(Pid).







-spec
stop(sockerl_types:name()) ->
    'ok'.
stop(Server) ->
    director:stop(Server, normal, ?DEFAULT_TERMINATE_TIMEOUT).







-spec
stop(sockerl_types:name(), Reason::any()) ->
    ok.
stop(Server, Reason) ->
    director:stop(Server, Reason, ?DEFAULT_TERMINATE_TIMEOUT).





%% ---------------------------------------------------------------------
%% 'director' callback:





%% @hidden
init({Mod, InitArg, Port, Opts}=Arg) ->
    TrMod = sockerl_utils:get_value(transporter
                                   ,Opts
                                   ,?DEFAULT_TRANSPORT_MODULE
                                   ,fun erlang:is_atom/1),
    case sockerl_socket:listen(TrMod, Port, Opts) of
        {ok, LSock} ->
            case catch Mod:listen_init(InitArg, LSock) of
                ok ->
                    {ok
                        ,[#{id => ?CONNECTION_SUP
                          ,start => {sockerl_server_connection_sup
                                    ,start_link
                                    ,[Mod, InitArg, Opts]}
                          ,type => supervisor
                          ,plan => [stop]
                          ,count => 1}
                        ,#{id => ?ACCEPTOR_SUP
                         ,start => {sockerl_acceptor_sup
                                   ,start_link
                                   ,[Opts, LSock]}
                         ,type => supervisor
                         ,plan => [stop]
                         ,count => 1}]};
                {ok, InitArg2} ->
                    {ok
                    ,[#{id => ?CONNECTION_SUP
                       ,start => {sockerl_server_connection_sup
                                 ,start_link
                                 ,[Mod, InitArg2, Opts]}
                       ,type => supervisor
                       ,plan => [stop]
                       ,count => 1}
                     ,#{id => ?ACCEPTOR_SUP
                       ,start => {sockerl_acceptor_sup
                                 ,start_link
                                 ,[Opts, LSock]}
                       ,type => supervisor
                       ,plan => [stop]
                       ,count => 1}]};
                ignore ->
                    ignore;
                {stop, _Reason}=Stop ->
                    Stop;
                {'EXIT', Reason} ->
                    {stop, Reason};
                Other ->
                    {stop
                    ,{bad_return_value, [{returned_value, Other}
                                        ,{module, Mod}
                                        ,{function, listen_init}
                                        ,{argument, Arg}]}}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.





%% ---------------------------------------------------------------------
%% Internal functions:





continue_starting(Opts, Pid) ->
    AccCount =
        sockerl_utils:get_value(acceptor_count
                               ,Opts
                               ,?DEFAULT_ACCEPTOR_COUNT
                               ,fun sockerl_utils:is_whole_integer/1),
    {ok, ConRootSupPid} = director:get_pid(Pid, ?CONNECTION_SUP),
    ConSups = start_connection_sups(AccCount, ConRootSupPid),
    {ok, AccSupPid} = director:get_pid(Pid, ?ACCEPTOR_SUP),
    ok = start_acceptors(ConSups, AccSupPid),
    {ok, Pid}.







start_connection_sups(AccCount, ConRootSupPid) ->
    start_connection_sups(AccCount, ConRootSupPid, []).

start_connection_sups(0, _ConRootSupPid, ConSups) ->
    ConSups;
start_connection_sups(Count, ConRootSupPid, ConSups) ->
    {ok, ConSupPid} = sockerl_server_connection_sup:add(ConRootSupPid
                                                       ,Count),
    start_connection_sups(Count-1
                         ,ConRootSupPid
                         ,[{Count, ConSupPid}|ConSups]).







start_acceptors(ConSups, AccSupPid) ->
    _ =
        [sockerl_acceptor_sup:add(ConSup, AccSupPid, Id)
        || {Id, ConSup} <- ConSups],
    ok.