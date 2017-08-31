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
%% ---------------------------------------------------------------------


-module(sockerl_acceptor_sup).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([start_link/2
        ,fetch/1
        ,sleep/1
        ,wakeup/1
        ,get_mode/1
        ,change_mode/1]).





%% for other sockerl modules:
-export([add/3]).





%% 'director' callback:
-export([init/1]).






-define(DEF_START_OPTS, [{log_validate_fun, fun log_validate/2}]).


%% ---------------------------------------------------------------------
%% API functions:





-spec
start_link(sockerl_types:start_options(), sockerl_types:socket()) ->
    sockerl_types:start_return().
start_link(Opts, LSock) ->
    director:start_link(?MODULE, {Opts, LSock}, ?DEF_START_OPTS).







-spec
fetch(sockerl_types:name()) ->
    [] | [{Id::term(), pid()}].
%% @doc
%%      returns all acceptors.
%% @end
fetch(AccSup) ->
    director:get_pids(AccSup).







-spec
sleep(sockerl_types:name()) ->
    'ok'.
sleep(AccSup) ->
    [sockerl_acceptor:sleep(Pid)
    || {_Id, Pid} <- director:get_pids(AccSup)],
    ok.







-spec
wakeup(sockerl_types:name()) ->
    'ok'.
wakeup(AccSup) ->
    [sockerl_acceptor:wakeup(Pid)
    || {_Id, Pid} <- director:get_pids(AccSup)],
    ok.







-spec
get_mode(sockerl_types:name()) ->
    sockerl_types:acceptor_mode()                   |
    [{pos_integer(), sockerl_types:acceptor_mode()}].
get_mode(AccSup) ->
    Modes =
        [{Id, sockerl_acceptor:get_mode(Pid)}
        || {Id, Pid} <- fetch(AccSup)],
    get_mode_fix_return(Modes).







-spec
change_mode(sockerl_types:name()) ->
    sockerl_types:acceptor_mode() | 'not_allowed'.
change_mode(AccSup) ->
    case get_mode(AccSup) of
        Modes when erlang:is_list(Modes) ->
            not_allowed;
        _Mode ->
            erlang:hd([sockerl_acceptor:change_mode(Pid)
                      || {_Id, Pid} <- fetch(AccSup)])
    end.







-spec
add(Pool::sockerl_types:name(), sockerl_types:name(), term()) ->
    sockerl_types:start_return().
add(ConSup, AccSup, Id) ->
    director:start_child(AccSup
                        ,#{id => Id
                          ,start => {sockerl_acceptor
                                    ,start_link
                                    ,[ConSup]}
                          ,append => true
                          ,plan => []
                          ,count => 1}).





%% ---------------------------------------------------------------------
%% 'director' callback:




%% @hidden
init({Opts, LSock}) ->
    {ok
    ,[]
    ,#{start => {sockerl_acceptor
                ,start_link
                ,[Opts, LSock]}
      ,plan => [stop]}}.





%% ---------------------------------------------------------------------
%% Internal functions:





get_mode_fix_return([{_, Mode}|Modes]=Modes2) ->
    get_mode_fix_return(Modes, Modes2, Mode).







get_mode_fix_return([{_, Mode}|Modes], Modes2, Mode) ->
    get_mode_fix_return(Modes, Modes2, Mode);

get_mode_fix_return([], _Modes, Mode) ->
    Mode;

get_mode_fix_return([_|_Modes], Modes2, _Mode) ->
    Modes2.






log_validate('$director', {warning, _}) ->
    long;
log_validate(_, {info, start}) ->
    none;
log_validate(_, {error, normal}) ->
    none;
log_validate(_, _) ->
    long.