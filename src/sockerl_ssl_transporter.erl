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
%%          API functions for ssl.
%% @end
%% ---------------------------------------------------------------------


-module(sockerl_ssl_transporter).
-author("pouriya.jahanbakhsh@gmail.com").
-behaviour(sockerl_transporter).


%% ---------------------------------------------------------------------
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





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





-define(DEFAULT_ACCEPTOR_ACCEPT_TIMEOUT, infinity).
-define(DEFAULT_ACCEPTOR_HANDSHAKE_TIMEOUT, infinity).
-define(DEFAULT_SOCKET_OPTIONS, []).
-define(DEFAULT_CONNECT_TIMEOUT, 3000).





%% ---------------------------------------------------------------------
%% API:





-spec
listen(sockerl_types:port_number()
      ,sockerl_types:start_options()) ->
    {'ok', sockerl_types:socket()} | sockerl_types:error().
listen(Port, Opts) ->
    ssl:listen(Port, get_socket_options(Opts)).







-spec
accept(sockerl_types:socket(), sockerl_types:start_options()) ->
    {ok, sockerl_types:socket()} | sockerl_types:error().
accept(ListenSock, Opts) ->
    {ATimeout, HTimeout} = get_accept_and_handshake_timeout(Opts),
    case catch ssl:transport_accept(ListenSock, ATimeout) of
        {ok, Sock} ->
            case catch ssl:ssl_accept(Sock, HTimeout) of
                ok ->
                    {ok, Sock};
                {error, Reason} ->
                    {error, {handshake, Reason}}
            end;
        Error ->
            Error
    end.







-spec
is_active(sockerl_types:socket()
         ,sockerl_types:start_options()) ->
    {ok, boolean()} | sockerl_types:error().
is_active(Sock, _Opts) ->
    case ssl:getopts(Sock, [active]) of
        {ok, [{_, Bool}]} ->
            {ok, Bool};
        {ok, []} ->
            {error, unknown_activity};
        {error, _Reason}=Error ->
            Error
    end.







-spec
recv(sockerl_types:socket()
    ,sockerl_types:length()
    ,timeout()
    ,sockerl_types:start_options()) ->
    {ok, sockerl_types:packet()} | sockerl_types:error().
recv(Sock, Len, Timeout, _Opts) ->
    ssl:recv(Sock, Len, Timeout).







-spec
is_socket_message(any(), sockerl_types:start_options()) ->
    {'true', sockerl_types:packet()} |
    {'true', sockerl_types:error()}                      |
    'false'.
is_socket_message({ssl, _Sock, Packet}, _Opts) ->
    {true, Packet};

is_socket_message({ssl_closed, _Sock}, _Opts) ->
    {true, {error, closed}};

is_socket_message({ssl_error, _Sock, Reason}, _Opts) ->
    {true, {error, Reason}};

is_socket_message(_Other, _Opts) ->
    false.







-spec
send(sockerl_types:socket()
    ,sockerl_types:packet()
    ,sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
send(Sock, Packet, _Opts) ->
    ssl:send(Sock, Packet).







-spec
setopts(sockerl_types:socket()
       ,list()
       ,sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
setopts(Sock, SockOpts, _Opts) ->
    ssl:setopts(Sock, SockOpts).







-spec
close(sockerl_types:socket(), sockerl_types:start_options()) ->
    ok | sockerl_types:error().
close(Sock, _Opts) ->
    ssl:close(Sock).







-spec
shutdown(sockerl_types:socket()
        ,sockerl_types:shutdown_type()
        ,sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
shutdown(Sock, Type, _Opts) ->
    ssl:shutdown(Sock, Type).







-spec
controlling_process(sockerl_types:socket()
                   ,pid()
                   ,sockerl_types:start_options()) ->
    'ok' | sockerl_types:error().
controlling_process(Sock, Pid, _Opts) ->
    ssl:controlling_process(Sock, Pid).







-spec
connect(sockerl_types:host()
       ,sockerl_types:port_number()
       ,sockerl_types:start_options()) ->
    {'ok', sockerl_types:socket()} | sockerl_types:error().
connect(Host, Port, Opts) ->
    {SockOpts, CTimeout} = get_socket_options_and_connect_timeout(Opts),
    ssl:connect(Host, Port, SockOpts, CTimeout).







-spec
format_error(any()) ->
    sockerl_types:error_info().
format_error(closed) ->
    "Socket connection closed";
format_error(timeout) ->
    "Could not connect in given time";
format_error(Reason) ->
    ssl:format_error(Reason).





%% ---------------------------------------------------------------------
%% Internal functions:





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
    ?DEFAULT_SOCKET_OPTIONS.







get_socket_options_and_connect_timeout(Opts) ->
    get_socket_options_and_connect_timeout(Opts, undefined, undefined).






get_socket_options_and_connect_timeout(_Opts, {SockOpts}, {CTimeout}) ->
    {SockOpts, CTimeout};
get_socket_options_and_connect_timeout([{socket_options, SockOpts}|Rest]
                                      ,_SockOpts
                                      ,CTimeout) ->
    case sockerl_utils:filter_socket_options(SockOpts) of
        ok ->
            get_socket_options_and_connect_timeout(Rest
                                                  ,{SockOpts}
                                                  ,CTimeout);
        {error, _Reason}=Error ->
            Error
    end;
get_socket_options_and_connect_timeout([{connect_timeout, CTimeout}
                                       |Rest]
                                      ,SockOpts
                                      ,_CTimeout) ->
    get_socket_options_and_connect_timeout(Rest, SockOpts, {CTimeout});
get_socket_options_and_connect_timeout([_|Rest], SockOpts, CTimeout) ->
    get_socket_options_and_connect_timeout(Rest, SockOpts, CTimeout);
get_socket_options_and_connect_timeout([], SockOpts, CTimeout) ->
    {case SockOpts of
         {SockOpts2} ->
             SockOpts2;
         undefined ->
             ?DEFAULT_SOCKET_OPTIONS
     end
        ,case CTimeout of
             {CTimeout2} ->
                 CTimeout2;
             undefined ->
                 ?DEFAULT_CONNECT_TIMEOUT
         end}.







get_accept_and_handshake_timeout(Opts) ->
    get_accept_and_handshake_timeout(Opts, undefined, undefined).






get_accept_and_handshake_timeout(_Opts, {ATimeout}, {HTimeout}) ->
    {ATimeout, HTimeout};
get_accept_and_handshake_timeout([{acceptor_accept_timeout, ATimeout}|Rest]
                                      ,_ATimeout
                                      ,HTimeout) ->
    get_accept_and_handshake_timeout(Rest, {ATimeout}, HTimeout);
get_accept_and_handshake_timeout([{acceptor_handshake_timeout, HTimeout}
                                       |Rest]
                                      ,ATimeout
                                      ,_CTimeout) ->
    get_accept_and_handshake_timeout(Rest, ATimeout, {HTimeout});
get_accept_and_handshake_timeout([_|Rest], ATimeout, HTimeout) ->
    get_accept_and_handshake_timeout(Rest, ATimeout, HTimeout);
get_accept_and_handshake_timeout([], ATimeout, HTimeout) ->
    {case ATimeout of
         {ATimeout2} ->
             ATimeout2;
         undefined ->
             ?DEFAULT_ACCEPTOR_ACCEPT_TIMEOUT
     end
        ,case HTimeout of
             {HTimeout2} ->
                 HTimeout2;
             undefined ->
                 ?DEFAULT_ACCEPTOR_HANDSHAKE_TIMEOUT
         end}.