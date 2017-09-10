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


-module(sockerl_metadata).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Exports:





%% API:
-export([wrap/9
        ,unwrap/1

        ,get_socket/1
        ,get_timeout/1
        ,get_srtimeout/1
        ,get_length/1
        ,get_transporter/1
        ,get_options/1
        ,get_last_message/1
        ,get_last_callback/1
        ,get_address/1]).





%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:





-include("internal/sockerl_metadata.hrl").





%% -------------------------------------------------------------------------------------------------
%% API:





-spec
wrap(sockerl_types:socket()
    ,timeout()
    ,timeout()
    ,sockerl_types:length()
    ,module()
    ,sockerl_types:start_options()
    ,any()
    ,atom()
    ,sockerl_types:address()) ->
    sockerl_types:metadata().
wrap(Sock, Timeout, SRTimeout, Len, TrMod, Opts, LMsg, LCB, Addr) ->
    #sockerl_metadata{socket = Sock
                     ,timeout = Timeout
                     ,length = Len
                     ,srtimeout = SRTimeout
                     ,transporter = TrMod
                     ,options = Opts
                     ,last_message = LMsg
                     ,last_callback = LCB
                     ,address = Addr}.







-spec
unwrap(sockerl_types:metadata()) ->
    {sockerl_types:socket()
    ,timeout()
    ,timeout()
    ,sockerl_types:length()
    ,module()
    ,sockerl_types:start_options()
    ,any()
    ,atom()
    ,sockerl_types:address()}.
unwrap(#sockerl_metadata{socket = Sock
                        ,timeout = Timeout
                        ,length = Len
                        ,srtimeout = SRTimeout
                        ,transporter = TrMod
                        ,options = Opts
                        ,last_message = LMsg
                        ,last_callback = LCB
                        ,address = Addr}) ->
    {Sock, Timeout, Len, SRTimeout, TrMod, Opts, LMsg, LCB, Addr}.







-spec
get_socket(sockerl_types:metadata()) ->
    sockerl_types:socket().
get_socket(#sockerl_metadata{socket = Sock}) ->
    Sock.







-spec
get_timeout(sockerl_types:metadata()) ->
    timeout().
get_timeout(#sockerl_metadata{timeout = Timeout}) ->
    Timeout.







-spec
get_srtimeout(sockerl_types:metadata()) ->
    timeout().
get_srtimeout(#sockerl_metadata{srtimeout = SRTimeout}) ->
    SRTimeout.







-spec
get_length(sockerl_types:metadata()) ->
    sockerl_types:length().
get_length(#sockerl_metadata{length = Len}) ->
    Len.







-spec
get_transporter(sockerl_types:metadata()) ->
    module().
get_transporter(#sockerl_metadata{transporter = TrMod}) ->
    TrMod.







-spec
get_options(sockerl_types:metadata()) ->
    sockerl_types:start_options().
get_options(#sockerl_metadata{options = Opts}) ->
    Opts.







-spec
get_last_message(sockerl_types:metadata()) ->
    any().
get_last_message(#sockerl_metadata{last_message = LMsg}) ->
    LMsg.







-spec
get_last_callback(sockerl_types:metadata()) ->
    atom().
get_last_callback(#sockerl_metadata{last_callback = LCB}) ->
    LCB.







-spec
get_address(sockerl_types:metadata()) ->
    sockerl_types:address().
get_address(#sockerl_metadata{address = Addr}) ->
    Addr.