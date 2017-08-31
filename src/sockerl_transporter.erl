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
%% @version 17.9
%% @hidden
%% -------------------------------------------------------------------------------------------------


-module(sockerl_transporter).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Behaviour info:





-callback
listen(Port, Opts) ->
    {'ok', ListenSock} | {'error', Reason}
when
    Port ::  sockerl_types:port_number(),
    Opts ::  sockerl_types:start_options(),
    ListenSock ::  sockerl_types:socket(),
    Reason :: any().







-callback
accept(ListenSocket, Opts) ->
    {'ok', Socket} | {'error', Reason}
when
    ListenSocket ::  sockerl_types:socket(),
    Opts ::  sockerl_types:start_options(),
    Socket ::  sockerl_types:socket(),
    Reason :: any().







-callback
connect(Host, Port, Opts) ->
    {'ok', Socket} | {'error', Reason}
when
    Host ::  sockerl_types:host(),
    Port ::  sockerl_types:port_number(),
    Opts ::  sockerl_types:start_options(),
    Socket ::  sockerl_types:socket(),
    Reason :: any().







-callback
send(Socket, Packet, Opts) ->
    'ok' | {'error', Reason}
when
    Socket ::  sockerl_types:socket(),
    Packet ::  sockerl_types:packet(),
    Opts ::  sockerl_types:start_options(),
    Reason :: any().







-callback
recv(Socket, Length, Timeout, Opts) ->
    {'ok', Packet} | {'error', Reason}
when
    Socket ::  sockerl_types:socket(),
    Length ::  sockerl_types:length(),
    Timeout :: timeout(),
    Opts ::  sockerl_types:start_options(),
    Packet ::  sockerl_types:packet(),
    Reason :: any().







-callback
setopts(Socket, SocketOpts, Opts) ->
    'ok' | {'error', Reason}
when
    Socket :: sockerl_types:socket(),
    SocketOpts :: list(),
    Opts :: sockerl_types:start_options(),
    Reason :: any().







-callback
close(Socket, Opts) ->
    'ok' | {'error', Reason}
when
    Socket ::  sockerl_types:socket(),
    Opts ::  sockerl_types:start_options(),
    Reason :: any().







-callback
shutdown(Socket, Type, Opts) ->
    'ok' | {'error', Reason}
when
    Socket ::  sockerl_types:socket(),
    Type ::  sockerl_types:shutdown_type(),
    Opts ::  sockerl_types:start_options(),
    Reason :: any().







-callback
format_error(Reason) ->
    Info
when
    Reason :: any(),
    Info :: sockerl_types:error_info().







-callback
is_active(Socket, Opts) ->
    {'ok', Bool} | {'error', Reason}
when
    Socket ::  sockerl_types:socket(),
    Opts ::  sockerl_types:start_options(),
    Bool :: boolean(),
    Reason :: any().







-callback
is_socket_message(Msg, Opts) ->
    {'true', Packet} | {'true', {'error', Reason}} | 'false'
when
    Msg :: any(),
    Opts ::  sockerl_types:start_options(),
    Packet ::  sockerl_types:packet(),
    Reason :: any().







-callback
controlling_process(Socket, Proc, Opts) ->
    'ok' | {'error', Reason}
when
    Socket ::  sockerl_types:socket(),
    Proc :: pid(),
    Opts ::  sockerl_types:start_options(),
    Reason :: any().