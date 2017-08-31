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
%% -------------------------------------------------------------------------------------------------


-module(sockerl_types).
-author("pouriya.jahanbakhsh@gmail.com").


%% -------------------------------------------------------------------------------------------------
%% Records & Macros & Includes:





-include("internal/sockerl_metadata.hrl").





%% -------------------------------------------------------------------------------------------------
%% Types:





-type name() :: pid() | atom().

-type start_return() :: {'ok', pid()} | {'error', term()} | 'ignore'.

-type register_name() :: {'local', atom()}
                       | {'global', atom()}
                       | {'via', module(), term()}.

-type error() :: {'error', {atom(), list()}} | {'error', term()}.

-type socket() :: any(). % Depends on transporter module

-type start_options() :: [] | [start_option()].
-type  start_option() :: {'acceptor_count', non_neg_integer()}
                       | {'acceptor_mode', 'accept' | 'sleep'}
                       | {'acceptor_debug', [sys:dbg_opt()]}
                       | {'connector_childspec_plan', list()}
                       | {'connector_childspec_count', non_neg_integer()
                                                     | 'infnity'}
                       | {'connector_per_address'
                         ,non_neg_integer()}
                       | {'transporter', module()}
                       | {'connector_debug', [sys:dbg_opt()]}
                       | {'socket_options', list()}
                       | any().

-type port_number() :: inet:port_number().

-type host() :: inet:hostname().

-type addresses() :: [] | [address()].
-type  address() :: {host(), port_number()}
                  | {'undefined', 'undefined'}.

-type packet() :: any(). %% Depends on transporter module

-type shutdown_type() :: 'read' | 'write' | 'read_write'.

-type error_info() :: string() | binary().

-type length() :: integer().

-type metadata() :: #sockerl_metadata{}.

-type acceptor_mode() :: 'accept' | 'sleep'.





-export_type([name/0
             ,start_return/0
             ,register_name/0
             ,error/0
             ,socket/0
             ,start_options/0
             ,start_option/0
             ,port_number/0
             ,host/0
             ,packet/0
             ,shutdown_type/0
             ,error_info/0
             ,length/0
             ,metadata/0
             ,acceptor_mode/0
             ,addresses/0
             ,address/0]).