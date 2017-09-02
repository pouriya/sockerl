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
%% @version 17.9.2
%% @hidden
%% -------------------------------------------------------------------------------------------------


-module(sockerl_tcp_transporter).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(sockerl_transporter).


%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([listen/2
        ,accept/2
        ,send/3
        ,recv/4
        ,connect/3
        ,setopts/3
        ,controlling_process/3
        ,is_active/2
        ,is_socket_message/2
        ,shutdown/3
        ,close/2
        ,format_error/1]).





%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:





-define(DEF_ACCEPTOR_ACCEPT_TIMEOUT, infinity).
-define(DEF_SOCKET_OPTIONS, []).
-define(DEF_CONNECT_TIMEOUT, 3000).





%% -------------------------------------------------------------------------------------------------
%% API:





-spec
listen(sockerl_types:port_number(), sockerl_types:start_options()) ->
    {'ok', sockerl_types:socket()} | sockerl_types:error().
listen(Port, Opts) ->
    gen_tcp:listen(Port, get_socket_options(Opts)).







-spec
accept(sockerl_types:socket(), sockerl_types:start_options()) ->
    {ok, sockerl_types:socket()} | sockerl_types:error().
accept(ListenSock, Opts) ->
    gen_tcp:accept(ListenSock, get_accept_timeout(Opts)).







-spec
is_active(sockerl_types:socket(), sockerl_types:start_options()) ->
    {ok, boolean()} | sockerl_types:error().
is_active(Sock, _Opts) ->
    case inet:getopts(Sock, [active]) of
        {ok, [{_, Bool}]} ->
            {ok, Bool};
        {ok, []} ->
            {error, unknown_activity};
        {error, _Reason}=Error ->
            Error
    end.







-spec
recv(sockerl_types:socket(), sockerl_types:length(), timeout(), sockerl_types:start_options()) ->
    {'ok', sockerl_types:packet()} | sockerl_types:error().
recv(Sock, Len, Timeout, _Opts) ->
    gen_tcp:recv(Sock, Len, Timeout).







-spec
is_socket_message(any(), sockerl_types:start_options()) ->
    {'true', sockerl_types:packet()} | {'true', sockerl_types:error()} | 'false'.
is_socket_message({tcp, _Sock, Packet}, _Opts) ->
    {true, Packet};

is_socket_message({tcp_closed, _Sock}, _Opts) ->
    {true, {error, closed}}; 

is_socket_message({tcp_error, _Sock, Reason}, _Opts) ->
    {true, {error, Reason}};

is_socket_message(_Other, _Opts) ->
    false.







-spec
send(sockerl_types:socket(), sockerl_types:packet(), sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
send(Sock, Packet, _Opts) ->
    gen_tcp:send(Sock, Packet).







-spec
setopts(sockerl_types:socket(), list(), sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
setopts(Sock, SockOpts, _Opts) ->
    inet:setopts(Sock, SockOpts).







-spec
close(sockerl_types:socket(), sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
close(Sock, _Opts) ->
    gen_tcp:close(Sock).







-spec
shutdown(sockerl_types:socket(), sockerl_types:shutdown_type(), sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
shutdown(Sock, Type, _Opts) ->
    gen_tcp:shutdown(Sock, Type).







-spec
controlling_process(sockerl_types:socket(), pid(), sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
controlling_process(Sock, Pid, _Opts) ->
    gen_tcp:controlling_process(Sock, Pid).







-spec
connect(sockerl_types:host(), sockerl_types:port_number(), sockerl_types:start_options()) ->
    {'ok', sockerl_types:socket()} | sockerl_types:error().
connect(Host, Port, Opts) ->
    {SockOpts, CTimeout} = get_socket_options_and_connect_timeout(Opts),
    gen_tcp:connect(Host, Port, SockOpts, CTimeout).







-spec
format_error(any()) ->
    sockerl_types:error_info().
format_error(closed) ->
    "Socket connection closed";
format_error(timeout) ->
    "Could not connect in given time";
format_error(Reason) ->
    inet:format_error(Reason).





%% -------------------------------------------------------------------------------------------------
%% Internal functions:





get_accept_timeout([{acceptor_accept_timeout, ATimeout}|_Rest]) ->
    ATimeout;
get_accept_timeout([_|Rest]) ->
    get_accept_timeout(Rest);
get_accept_timeout([]) ->
    ?DEF_ACCEPTOR_ACCEPT_TIMEOUT.







get_socket_options([{socket_options, SockOpts}|_Rest]) ->
    case sockerl_utils:filter_socket_options(SockOpts) of
        ok ->
            SockOpts;
        {error, _Reason}=Error ->
            Error
    end;
get_socket_options([_|Rest]) ->
    get_socket_options(Rest);
get_socket_options([]) ->
    ?DEF_SOCKET_OPTIONS.







get_socket_options_and_connect_timeout(Opts) ->
    get_socket_options_and_connect_timeout(Opts, undefined, undefined).






get_socket_options_and_connect_timeout(_Opts, {SockOpts}, {CTimeout}) ->
    {SockOpts, CTimeout};
get_socket_options_and_connect_timeout([{socket_options, SockOpts} | Rest], _SockOpts, CTimeout) ->
    case sockerl_utils:filter_socket_options(SockOpts) of
        ok ->
            get_socket_options_and_connect_timeout(Rest, {SockOpts}, CTimeout);
        {error, _Reason}=Error ->
            Error
    end;
get_socket_options_and_connect_timeout([{connect_timeout, CTimeout} | Rest], SockOpts, _CTimeout) ->
    get_socket_options_and_connect_timeout(Rest, SockOpts, {CTimeout});
get_socket_options_and_connect_timeout([_|Rest], SockOpts, CTimeout) ->
    get_socket_options_and_connect_timeout(Rest, SockOpts, CTimeout);
get_socket_options_and_connect_timeout([], SockOpts, CTimeout) ->
    {case SockOpts of
         {SockOpts2} ->
             SockOpts2;
         undefined ->
             ?DEF_SOCKET_OPTIONS
     end
    ,case CTimeout of
         {CTimeout2} ->
             CTimeout2;
         undefined ->
             ?DEF_CONNECT_TIMEOUT
     end}.
