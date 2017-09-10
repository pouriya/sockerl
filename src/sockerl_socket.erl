%%% ------------------------------------------------------------------------------------------------
%%% Sockerl is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------
%% @author  Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version 17.9.10
%% -------------------------------------------------------------------------------------------------


-module(sockerl_socket).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([listen/3
        ,accept/3
        ,send/4
        ,recv/5
        ,connect/4
        ,setopts/4
        ,controlling_process/4
        ,is_active/3
        ,is_socket_message/3
        ,shutdown/4
        ,close/3
        ,format_error/2]).





%% -------------------------------------------------------------------------------------------------
%% API:





-spec
listen(module(), sockerl_types:port_number(), sockerl_types:start_options()) ->
    {'ok', sockerl_types:socket()} | sockerl_types:error().
%% @doc
%%      returns listen socket which can accepts incoming connections on
%%      given port.
%% @end
listen(TrMod, Port, Opts) ->
    case catch TrMod:listen(Port, Opts) of
        {ok, ListenSock} ->
            {ok, ListenSock};
        {error, Reason} ->
            {error, {socket_listen, [{reason, Reason}
                                    ,{info, format_error(TrMod, Reason)}
                                    ,{transporter, TrMod}
                                    ,{port, Port}
                                    ,{options, Opts}]}};
        {'EXIT', Reason} ->
            {error, {socket_listen_crash, [{reason, Reason}
                                          ,{transporter, TrMod}
                                          ,{port, Port}
                                          ,{options, Opts}]}};
        Other ->
            {error, {socket_listen_return, [{returned_value, Other}
                                           ,{transporter, TrMod}
                                           ,{port, Port}
                                           ,{options, Opts}]}}
    end.







-spec
accept(module(), sockerl_types:socket(), sockerl_types:start_options()) ->
    {ok, sockerl_types:socket()} | sockerl_types:error().
%% @doc
%%      accepts incoming connection and returns new connection socket.
%% @end
accept(TrMod, LSock, Opts) ->
    case catch TrMod:accept(LSock, Opts) of
        {ok, _Sock}=Ok ->
            Ok;
        {error, Reason} ->
            {error, {socket_accept, [{reason, Reason}
                                    ,{info, format_error(TrMod, Reason)}
                                    ,{transporter, TrMod}
                                    ,{listen_socket, LSock}
                                    ,{options, Opts}]}};
        {'EXIT', Reason} ->
            {error, {socket_accept_crash, [{reason, Reason}
                                          ,{transporter, TrMod}
                                          ,{listen_socket, LSock}
                                          ,{options, Opts}]}};
        Other ->
            {error, {socket_accept_return, [{returned_value, Other}
                                           ,{transporter, TrMod}
                                           ,{listen_socket, LSock}
                                           ,{options, Opts}]}}
    end.







-spec
is_active(module(), sockerl_types:socket(), sockerl_types:start_options()) ->
    {ok, boolean()} | sockerl_types:error().
%% @doc
%%      returns activity flag of socket.
%% @end
is_active(TrMod, Sock, Opts) ->
    case catch TrMod:is_active(Sock, Opts) of
        {ok, _Bool}=Ok ->
            Ok;
        {error, Reason} ->
            {error, {socket_get_activity, [{reason, Reason}
                                          ,{info, format_error(TrMod, Reason)}
                                          ,{transporter, TrMod}
                                          ,{socket, Sock}]}};
        {'EXIT', Reason} ->
            {error, {socket_get_activity_crash, [{reason, Reason}
                                                ,{transporter, TrMod}
                                                ,{socket, Sock}]}};
        Other ->
            {error, {socket_get_activity_return, [{returned_value, Other}
                                                 ,{transporter, TrMod}
                                                 ,{socket, Sock}]}}
    end.







-spec
recv(module()
    ,sockerl_types:socket()
    ,sockerl_transporter:length()
    ,timeout()
    ,sockerl_types:start_options()) ->
    {ok, sockerl_types:packet()} | sockerl_types:error().
%% @doc
%%      Receives data from passive socket.
%% @end
recv(TrMod, Sock, Len, Timeout, Opts) ->
    case catch TrMod:recv(Sock, Len, Timeout, Opts) of
        {ok, _Packet}=Ok ->
            Ok;
        {error, Reason} ->
            {error, {socket_recv, [{reason, Reason}
                                  ,{info, format_error(TrMod, Reason)}
                                  ,{transporter, TrMod}
                                  ,{socket, Sock}
                                  ,{options, Opts}]}};
        {'EXIT', Reason} ->
            {error, {socket_recv_crash, [{reason, Reason}
                                        ,{transporter, TrMod}
                                        ,{socket, Sock}
                                        ,{options, Opts}]}};
        Other ->
            {error, {socket_recv_return, [{returned_value, Other}
                                         ,{transporter, TrMod}
                                         ,{socket, Sock}
                                         ,{options, Opts}]}}
    end.







-spec
is_socket_message(module(), term(), sockerl_types:start_options()) ->
    {ok, sockerl_types:packet()} | sockerl_types:error() | 'false'.
%% @doc
%%      determines that the term is message like messages of gen_tcp
%%      and ssl sockets in active mode or not.
%% @end
is_socket_message(TrMod, Msg, Opts) ->
    case catch TrMod:is_socket_message(Msg, Opts) of
        {true, {error, Reason}} ->
            {error, {socket_check_message, [{reason, Reason}
                                           ,{info, format_error(TrMod, Reason)}
                                           ,{transporter, TrMod}
                                           ,{message, Msg}]}};
        {true, Packet} when erlang:is_binary(Packet) orelse
                            erlang:is_list(Packet) ->
            {ok, Packet};
        false ->
            false;
        {true, Packet} ->
            {error, {socket_check_message, [{reason, packet_type}
                                           ,{info, "Bad packet type"}
                                           ,{transporter, TrMod}
                                           ,{message, Msg}
                                           ,{packet, Packet}]}};
        {'EXIT', Reason} ->
            {error, {socket_check_message_crash, [{reason, Reason}
                                                 ,{transporter, TrMod}
                                                 ,{message, Msg}]}};
        Other ->
            {error, {socket_check_message_return, [{returned_value, Other}
                                                  ,{transporter, TrMod}
                                                  ,{message, Msg}]}}
    end.







-spec
send(module()
    ,sockerl_types:socket()
    ,sockerl_types:packet()
    ,sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
%% @doc
%%      sends packet through socket.
%% @end
send(TrMod, Sock, Packet, Opts) ->
    case catch TrMod:send(Sock, Packet, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {socket_send, [{reason, Reason}
                                  ,{info, format_error(TrMod, Reason)}
                                  ,{socket, Sock}
                                  ,{packet, Packet}
                                  ,{options, Opts}]}};
        {'EXIT', Reason} ->
            {error, {socket_send_crash, [{reason, Reason}
                                        ,{socket, Sock}
                                        ,{packet, Packet}
                                        ,{options, Opts}]}};
        Other ->
            {error, {socket_send_return, [{returned_value, Other}
                                         ,{socket, Sock}
                                         ,{packet, Packet}
                                         ,{options, Opts}]}}
    end.







-spec
setopts(module(), sockerl_types:socket(), list(), sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
%% @doc
%%      Changes options of socket.
%% @end
setopts(TrMod, Sock, SockOpts, Opts) ->
    case catch TrMod:setopts(Sock, SockOpts, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {socket_setopts, [{reason, Reason}
                                     ,{info, format_error(TrMod, Reason)}
                                     ,{socket, Sock}
                                     ,{socket_options, SockOpts}
                                     ,{options, Opts}]}};
        {'EXIT', Reason} ->
            {error, {socket_setopts_crash, [{reason, Reason}
                                           ,{socket, Sock}
                                           ,{socket_options, SockOpts}
                                           ,{options, Opts}]}};
        Other ->
            {error, {socket_setopts_return, [{returned_value, Other}
                                            ,{socket, Sock}
                                            ,{socket_options, SockOpts}
                                            ,{options, Opts}]}}
    end.







-spec
close(module(), sockerl_types:socket(), sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
%% @doc
%%      Closes an open socket.
%% @end
close(TrMod, Sock, Opts) ->
    case catch TrMod:close(Sock, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {socket_close, [{reason, Reason}
                                   ,{info, format_error(TrMod, Reason)}
                                   ,{transporter, TrMod}
                                   ,{socket, Sock}]}};
        {'EXIT', Reason} ->
            {error, {socket_close_crash, [{reason, Reason}
                                         ,{transporter, TrMod}
                                         ,{socket, Sock}]}};
        Other ->
            {error, {socket_close_return, [{returned_value, Other}
                                          ,{transporter, TrMod}
                                          ,{socket, Sock}]}}
    end.







-spec
shutdown(module()
        ,sockerl_types:socket()
        ,sockerl_types:shutdown_type()
        ,sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
%% @doc
%%      closes read or write or read/write of socket.
%% @end
shutdown(TrMod, Sock, Type, Opts) ->
    case catch TrMod:shutdown(Sock, Type, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {socket_shutdown, [{reason, Reason}
                                      ,{info, format_error(TrMod, Reason)}
                                      ,{transporter, TrMod}
                                      ,{socket, Sock}
                                      ,{type, Type}]}};
        {'EXIT', Reason} ->
            {error, {socket_shutdown_crash, [{reason, Reason}
                                            ,{transporter, TrMod}
                                            ,{socket, Sock}
                                            ,{type, Type}]}};
        Other ->
            {error, {socket_shutdown_return, [{returned_value, Other}
                                             ,{transporter, TrMod}
                                             ,{socket, Sock}
                                             ,{type, Type}]}}
    end.







-spec
controlling_process(module()
                   ,sockerl_types:socket()
                   ,pid()
                   ,sockerl_types:start_options()) ->
    'ok' |sockerl_types:error().
%% @doc
%%      changes the receiver of socket messages.
%% @end
controlling_process(TrMod, Sock, Pid, Opts) ->
    case catch TrMod:controlling_process(Sock, Pid, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {socket_controlling_process, [{reason, Reason}
                                                 ,{info, format_error(Sock, Reason)}
                                                 ,{transporter, TrMod}
                                                 ,{socket, Sock}
                                                 ,{pid, Pid}
                                                 ,{options, Opts}]}};
        {'EXIT', Reason} ->
            {error, {socket_controlling_process_crash, [{reason, Reason}
                                                       ,{transporter, TrMod}
                                                       ,{socket, Sock}
                                                       ,{pid, Pid}
                                                       ,{options, Opts}]}}
    end.







-spec
connect(module()
       ,sockerl_types:host()
       ,sockerl_types:port_number()
       ,sockerl_types:start_options()) ->
    {'ok', sockerl_types:socket()} |sockerl_types:error().
%% @doc
%%      makes a connection to remote host and port.
%% @end
connect(TrMod, Host, Port, Opts) ->
    case catch TrMod:connect(Host, Port, Opts) of
        {ok, Sock} ->
            {ok, Sock};
        {error, Reason} ->
            {error, {socket_connect, [{reason, Reason}
                                     ,{info, format_error(TrMod, Reason)}
                                     ,{transporter, TrMod}
                                     ,{host, Host}
                                     ,{port, Port}
                                     ,{options, Opts}]}};
        {'EXIT', Reason} ->
            {error, {socket_connect_crash, [{reason, Reason}
                                           ,{transporter, TrMod}
                                           ,{host, Host}
                                           ,{port, Port}
                                           ,{options, Opts}]}};
        Other ->
            {error, {socket_connect_return, [{returned_value, Other}
                                            ,{transporter, TrMod}
                                            ,{host, Host}
                                            ,{port, Port}
                                            ,{options, Opts}]}}
    end.










-spec
format_error(module(), any()) ->
    string() | binary().
%% @doc
%%      Returns understandable string or binary about error.
%% @end
format_error(TrMod, Reason) ->
    case catch TrMod:format_error(Reason) of
        Info when erlang:is_list(Info) orelse erlang:is_binary(Info) ->
            Info;
        _Other ->
            "Could not get error info from module '" ++ erlang:atom_to_list(TrMod) ++ "'"
    end.